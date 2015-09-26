# JSON API used by stack-ide

This package is split off from `stack-ide` for the following reasons:

* Support for compilation using [GHCJS](https://github.com/ghcjs/ghcjs).

* It has fewer dependencies than `stack-ide`. Notably, it only depends on
  `ide-backend-common`, which doesn't depend on ghc or Cabal and so is fairly
  trivial to install.
