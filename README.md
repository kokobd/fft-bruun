# fft-bruun

[![Build Status](https://travis-ci.org/IsumiF/fft-bruun.svg?branch=master)](https://travis-ci.org/IsumiF/fft-bruun)

FFT using Bruun's algorithm.

## Configure

This project use Cabal new build (nix style local build).

Make sure to have these tools installed:
- `cabal` version 2.4 or above
- `ghc` 8.6.2

On ubuntu, the easiest way is to use hvr's ppa. See [its page](https://launchpad.net/~hvr/+archive/ubuntu/ghc) for details.

If you haven't used these tools before, run `cabal v2-update` first.

Also, some system libraries need to be installed:

- libblas-dev
- liblapack-dev
- gsl-bin
- libgsl-dev

You may install them to non-default location, in that case you need to run
```bash
cabal v2-configure --extra-lib-dirs=xxx --extra-include-dirs=xxx
```
to tell cabal where the libraries are placed.

## Run Tests

```bash
cabal v2-test
```

## Run Benchmarks

```bash
cabal v2-bench
```
