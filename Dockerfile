#-- Haskell Stack non-privileged builder image.
#--
#-- Installs Stack from official signed binary,
#--   then GHC from musl bindist (via stack setup).
#--
#-- Usage:
#--   docker build -t haskell-stack-builder:latest .

FROM debian:bullseye-slim

#-- UTF-8 everywhere. No l10n.
ENV LC_ALL=C.UTF-8

#-- DL3008 -- we're okay with these deps autoupdated.
#-- DL3009 -- see hadolint issue #185.
# hadolint ignore=DL3008,DL3009
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    g++ \
    git \
    gnupg \
    libc-dev \
    libstdc++-10-dev \
    libgmp-dev \
    liblzma-dev \
    libncurses-dev \
    make \
    openssh-client \
    perl \
    pkg-config \
    sudo \
    tar \
    xz-utils \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Alpine has libtinfo linked into libncurses
#RUN ln -vsf libncursesw.so.6 /usr/lib/libtinfow.so.6

ARG STACK_RESOLVER=lts-18.17
ARG GHC_VERSION=implied

ENV STACK_BIN=https://github.com/commercialhaskell/stack/releases/download/v2.7.3/stack-2.7.3-linux-x86_64-bin
# Not using a STACK_VERSION variable, because they subtly changed
# the asset naming/url scheme between v2.3.1 and v2.3.3. Use sed and caution
WORKDIR /usr/local/share
COPY stack-2.7.3-linux-x86_64-bin.sha256 \
     stack-2.7.3-linux-x86_64-bin.asc \
     GPG-KEY-575159689BEFB442-dev@fpcomplete \
     ./
# hadolint ignore=DL4006
RUN curl -fsSL "$STACK_BIN" -o ${STACK_BIN##*/} && \
    sha256sum -c stack-2.7.3-linux-x86_64-bin.sha256 >&2 && \
    gpg -q --import GPG-KEY-* && \
    printf "trust\n5\ny\n" | gpg --command-fd 0 --no-tty --edit-key 575159689BEFB442 2>/dev/null && \
    gpg --verify stack-2.7.3-linux-x86_64-bin.asc && \
    rm -rf ~/.gnupg && \
    mv -v stack-*-linux-x86_64-bin /usr/local/bin/stack && \
    chmod 755 /usr/local/bin/stack && \
    echo "stack binary verified and installed." >&2 && \
    stack --version
# [+60 MiB]

ARG UID=1000
# hadolint ignore=SC2016
RUN adduser --disabled-password -u $UID builder --gecos "" && \
    echo 'builder ALL=(ALL) NOPASSWD: ALL' > /etc/sudoers.d/enable-builder-sudo && \
    sed -i 's:profile\.d/\*\.sh:profile.d/*:' /etc/profile && \
    printf 'test -d "$HOME/bin" && [ "${PATH#*$HOME/bin}" == "$PATH" ] && export PATH="$PATH:$HOME/bin"' > /etc/profile.d/add-home-bin-to-PATH && \
    printf 'test -d "$HOME/.local/bin" && [ "${PATH#*$HOME/.local/bin}" == "$PATH" ] && export PATH="$PATH:$HOME/.local/bin"' > /etc/profile.d/add-local-bin-to-PATH && \
    printf 'export PATH="$PATH:$(stack path --compiler-bin)"' > /etc/profile.d/add-stack-ghc-to-PATH && \
    { echo 'source /etc/bash.bashrc'; echo 'source /etc/profile'; } > /home/builder/.bashrc && \
    :
# workaround sudo bug 42 https://github.com/sudo-project/sudo/issues/42
RUN echo 'Set disable_coredump false' >> /etc/sudo.conf

#-- drop root
USER builder
WORKDIR /home/builder

#-- configure Stack, pull a GHC, strip it down [+1.34 GiB]
COPY --chown=builder stack-config.yaml /tmp/
#-- SC2046 -- word-splitting is intented @ GHC_VERSION
# hadolint ignore=SC2046
RUN \
    if [ "$GHC_VERSION" = "8.6.5" ]; then \
        sed -i 's!\(- --enable-executable-static\)!#\1 # requires Cabal 3.0+!' /tmp/stack-config.yaml; \
    fi && \
    install -Dm644 /tmp/stack-config.yaml /home/builder/.stack/config.yaml && \
    stack setup \
        --install-ghc \
        --resolver=$STACK_RESOLVER \
        $(test $GHC_VERSION = implied || echo $GHC_VERSION) \
        ; success=$?; \
    rm -rf ~/.stack/programs/*/ghc-*.tar.xz \
           ~/.stack/programs/*/ghc-*/share/doc \
        ; \
    strip ~/.stack/programs/*/ghc-*/lib/ghc-*/bin/* 2>/dev/null \
        ; \
    exit $success

#-- almost done; pre-download snapshot index for speed [+1.29 GiB]
RUN stack update

#-- setup PATH
# ... for posix-mode bash-invoked-as-/bin/sh
ENV ENV=/etc/profile
# ... for direct docker-run's skipping the shell
ENV PATH=/usr/bin:/bin:/usr/local/bin:/home/builder/bin:/home/builder/.local/bin:/home/builder/.stack/programs/x86_64-linux/ghc-tinfo6-8.10.7/bin
# FIXME there's no way to unhardcode the ghc version in PATH above. Vanilla Dockerfile is not flexible enough for this. Consider that the tinfo6 fragment ("ghc variant") may change too.


#-- add correctly permissioned volumes for host code & build outputs
RUN mkdir -p src bin
VOLUME /home/builder/src
VOLUME /home/builder/bin
WORKDIR /home/builder/src
