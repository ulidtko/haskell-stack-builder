# `haskell-stack-builder` #

This is an unprivileged (rootless) Docker Image for building Haskell using Stack.

GHC version: 8.6.5

Cabal version: 2.4.1.0

Stack version: 2.3.3

The image is `FROM alpine` but configured for building static binaries; build outputs will run on any other Linux distro. Musl libc is linked into those binaries.

## Basic usage, volumes ##

First build the image:

    docker build -t haskell-stack-builder:latest .

Then supposing you have a Stack project under `foobar_project` and `binary_outputs` directory:

    docker run --rm -t \
        -v $PWD/foobar_project:/home/builder/src \
        -v $PWD/binary_outputs:/home/builder/bin \
        haskell-stack-builder:latest \
        stack build --copy-bins

That's it. On build success, find the static executables under `binary_outputs/`.

## Size ##

**2.88 GiB** (3.09 GB):
 * 1.3 GiB for GHC,
 * 1.3 GiB for Stack package index,
 * 200 MiB for a few Alpine packages,
 * 60 MiB for Stack static binary.
