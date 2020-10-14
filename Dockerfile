#-- Haskell Stack non-privileged builder image.
#--
#-- Installs GHC and Cabal from Alpine repos,
#--   Stack from official signed binary.
#--
#-- Usage:
#--   docker build -t haskell-stack-builder:latest .

#-- Alpine 3.11 is the last version with GHC 8.6.5
FROM alpine:3.11

RUN apk update --no-cache && \
    apk add --no-cache \
        cabal \
        curl \
        ghc \
        gnupg \
        sudo \
        libc-dev \
        zlib-dev \
    ;

ENV STACK_BIN=https://github.com/commercialhaskell/stack/releases/download/v2.3.3/stack-2.3.3-linux-x86_64-bin
WORKDIR /usr/local/share
COPY stack-2.3.3-linux-x86_64-bin.sha256 \
     stack-2.3.3-linux-x86_64-bin.asc \
     GPG-KEY-65101FF31C5C154D-eborsboom@fpcomplete \
     ./
# hadolint ignore=DL4006
RUN curl -fsSL "$STACK_BIN" -o ${STACK_BIN##*/} && \
    sha256sum -c stack-*-linux-x86_64-bin.sha256 >&2 && \
    gpg -q --import GPG-KEY-* && \
    printf "trust\n5\ny\n" | gpg --command-fd 0 --no-tty --edit-key 65101FF31C5C154D 2>/dev/null && \
    gpg --verify stack-*-linux-x86_64-bin.asc && \
    rm -rf ~/.gnupg && \
    mv -v stack-*-linux-x86_64-bin /usr/local/bin/stack && \
    chmod 755 /usr/local/bin/stack && \
    echo "stack binary verified and installed." >&2 && \
    stack --version

#-- drop root
RUN adduser -D -u 1000 builder && \
    echo 'builder ALL=(ALL) NOPASSWD: ALL' > /etc/sudoers.d/enable-builder-sudo && \
    sed -i 's:profile\.d/\*\.sh:profile.d/*:' /etc/profile && \
    printf 'test -d "$HOME/bin" && [ "${PATH#*$HOME/bin}" == "$PATH" ] && export PATH="$PATH:$HOME/bin"' > /etc/profile.d/add-home-bin-to-PATH
# workaround sudo bug 42 https://github.com/sudo-project/sudo/issues/42
RUN echo 'Set disable_coredump false' >> /etc/sudo.conf
USER builder
WORKDIR /home/builder
ENV ENV=/etc/profile

#-- configure Cabal and Stack
COPY cabal-config.cabal stack-config.yaml /tmp/
RUN install -Dm644 /tmp/stack-config.yaml /home/builder/.stack/config.yaml && \
    install -Dm644 /tmp/cabal-config.cabal /home/builder/.cabal/config && \
    :

#-- add correctly permissioned volumes for host code & build outputs
RUN mkdir src bin
VOLUME /home/builder/src
VOLUME /home/builder/bin
WORKDIR /home/builder/src
