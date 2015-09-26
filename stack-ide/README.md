# Standalone JSON client for `ide-backend`

This executable binds against the `ide-backend` library and makes it API
available over JSON. It is intended to faciliate integration of `ide-backend` in
editors such Atom, Emacs, Sublime, Vim, etc.  This repository contains an emacs
integration called `stack-mode`.

# Project Status

stack-ide is still a work-in-progress.  The goal is to create an environment for
seamlessly getting lots of info about our Haskell code, but it isn't perfect
yet.  Give it a try, and feel free to join in on development!

# Installation instructions

Run `stack install` to locally install `ide-backend` and `stack-ide`.
