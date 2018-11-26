# fft-bruun

FFT using Bruun's algorithm.

## Configure

This project use Cabal new build (nix style local build).

Make sure to have these tools installed:
- `cabal` version 2.4 or above
- `ghc` 8.6.2
- MatLab R2018b (some tests depends on MatLab)

If you haven't used these tools before, run `cabal v2-update` first.

In this guide, we use Ubuntu 18.04. You could translate these commands to
Windows and OSX.

You need to tell the build system where your matlab installation is. Replace
`$MATLAB_HOME` with your MatLab installation root.
```bash
cabal v2-configure \
  --extra-include-dirs: $MATLAB_HOME/extern/include \
  --extra-lib-dirs: $MATLAB_HOME/extern/bin/glnxa64
```
For include dirs and lib dirs on other systems, please consult [MatLab
documentation][1].

Before actually running tests, benchmarks or other executables, make sure to
have `LD_LIBRARY_PATH` properly set in your shell. For example:
```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$MATLAB_HOME/extern/bin/glnxa64
# same as 'extra-lib-dirs'
```
At run time, `ld`, the usual dynamic linker of Linux, will look for MatLab's
shared library in system standard location AND `LD_LIBRARY_PATH`, so it's
essential to set this variable. We don't recommend add it to your `.bashrc` or
alike, as it may affect other applications.

To summarize:
```bash
# Fetch package indicies from Hackage
cabal v2-update
# Only need to be run once
cabal v2-configure \
  --extra-include-dirs: $MATLAB_HOME/extern/include \
  --extra-lib-dirs: $MATLAB_HOME/extern/bin/glnxa64
# Tell ld about the location of MatLab shared objects
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$MATLAB_HOME/extern/bin/glnxa64
```

## Run Tests

```haskell
cabal v2-test
```

[1]: https://www.mathworks.com/help/matlab/matlab_external/build-c-engine-programs.html
