# JSON API used by ide-backend-client

This package is split off from `ide-backend-client` for the following
reasons:

* Support for compilation using [GHCJS](https://github.com/ghcjs/ghcjs).

* It has fewer dependencies than `ide-backend-client`.  Notably, it
  only depends on `ide-backend-common`, which doesn't depend on ghc or
  Cabal and so is fairly trivial to install.
