# `haskell-stack-builder` #

This is an unprivileged (rootless) Docker Image for building Haskell using Stack.

Default build config:

 * GHC version: 8.10.7
 * Cabal version: 3.2.1.0
 * Stack version: 2.7.3
 * Stackage snapshot: LTS-18.28

Versions are customizable via `--build-arg`'s, see below.

## Basic usage, volumes ##

First build the image:

    docker build -t haskell-stack-builder:lts-18.28 .

Then supposing you have a Stack project under `foobar_project` and `binary_outputs` directory:

    docker run --rm -t \
        -v $PWD/foobar_project:/home/builder/src \
        -v $PWD/binary_outputs:/home/builder/bin \
        haskell-stack-builder:lts-18.28 \
        stack build --copy-bins

That's it. On build success, find the executables under `binary_outputs/`.

## Size ##

In LTS-14.27 build config & `FROM debian:buster-slim`:

**3.05 GiB** (3.28 GB):
 * 1.35 GiB for GHC,
 * 1.35 GiB for Stack package index,
 * 229 MiB for a few basic Debian packages,
 * 60 MiB for Stack static binary.

Extra effort has been applied to prune unnecessary stuff; see [Dockerfile](./Dockerfile).

## Build config options ##

With the primary purpose to minimize Dockerfile branching, these are accepted:

`--build-arg` key | Example values | Default value | Explanation
------------------|----------------|---------------|---------------------------
`GHC_VERSION`     | 8.8.2, 8.8.4   | 8.6.5         | Used in `stack setup --install-ghc`
`STACK_RESOLVER`  | lts-16.31      | lts-14.27     | Stackage snapshot tag
`UID`             | 1001, 65534    | 1000          | POSIX UID of `builder` user

Complete `docker build` commandline specifying all the options (example):

    docker build \
        --build-arg UID=$(id -u) \
        --build-arg GHC_VERSION=8.8.4 \
        --build-arg STACK_RESOLVER=lts-16.31 \
        -t haskell-stack-builder:lts-16.31 \
        .

## Dependency caching on CI ##

Unlike some [other builders][YARN_CACHE_FOLDER], Stack does not provide a clear-cut option to preserve compiled dependencies externally. This often leads to ridiculously long CI builds, as those hundreds of dependency packages get recompiled from scratch in every pipeline run.

A way around this is to pick apart the `~/.stack` directory, and bind-mount the relevant parts:

    docker run --rm -t \
        -v HOST_HASKELL_CACHE_DIR/stack.sqlite3:/home/builder/.stack/stack.sqlite3 \
        -v HOST_HASKELL_CACHE_DIR/snapshots:/home/builder/.stack/snapshots \
        \
        -v $PWD/foobar_project:/home/builder/src \
        -v $PWD/binary_outputs:/home/builder/bin \
        haskell-stack-builder:latest \
        stack build --copy-bins

`stack.sqlite3` is the package DB of Stack, there it remembers installed package metadata. `snapshots` is where build products go¹. **No need to cache anything else** under `~/.stack`: doing so can spawn weird issues (via stale `config.yaml`), or bloat your cache by several gigs unnecessarily (via accidentally caching a GHC under `programs`). The `pantry` subdir is important, somewhat heavy too, *should not* be cached — it contains effectively immutable index of available packages and is already included in the docker image. Per Stack's immutable snapshots architecture, refreshing pantry involves updating the `lts-14.27` tag, and thus rebuilding this image.

Look into the [lockfile][stack.yaml.lock] if you want to know the best **invalidation strategy** for such a cache. The lockfile varies across projects, but within one there's a nice property: `stack.yaml.lock` contents will change exactly on changes in the project's dependency forest, and will stay the same otherwise. Thus a hashsum of it makes a good caching key.

[YARN_CACHE_FOLDER]: https://classic.yarnpkg.com/en/docs/cli/cache/
[stack.yaml.lock]: https://docs.haskellstack.org/en/stable/lock_files/

\[¹\]: roughly speaking; there's also the `my_project/.stack-work` serving a similar purpose. The difference is of a local/global kind; once compiled, `my_project` modules (`Config.hs`, `Utils.hs`, what have you) will go under the project-local `.stack-work`; but dependency *packages* (e.g. `text`, `lens`, `aeson`) will go under the user-global `~/.stack/snapshots`. Doing so enjoys deterministic-build properties of packages in Stack, and facilitates built deps reuse across projects (so there won't appear gazillion builds of `text-1.2.3.1`, just a single one per `(cpu_architecture, compile_flag_set)` will exist).
