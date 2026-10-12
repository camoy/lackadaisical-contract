# Lackadaisical Contracts

[![Build Status][build-badge]][build]
[![Scribble][docs-badge]][docs]

Lackadaisical contracts provide a mechanism for deferred checking of higher-order interactions at the contract level.
See [the documentation][docs] for information on how to install and use the library.

## Install

```
$ raco pkg install lackadaisical-contract
```

## Organization

This repository is organized into two separate packages.

* `lackadaisical-contract/`
  contains the documentation for how to use the library
  and well as the unit tests for the implementation.
* `lackadaisical-contract-lib/`
  contains the main code for the library.

[build-badge]: https://github.com/camoy/lackadaisical-contract/actions/workflows/build.yml/badge.svg
[build]: https://github.com/camoy/lackadaisical-contract/actions/workflows/build.yml?query=workflow%3Abuild
[docs-badge]: https://img.shields.io/badge/Docs-Scribble-blue.svg
[docs]: https://docs.racket-lang.org/lackadaisical-contract
