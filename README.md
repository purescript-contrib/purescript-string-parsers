# String Parsers

[![CI](https://github.com/purescript-contrib/purescript-string-parsers/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-string-parsers/actions?query=workflow%3ACI+branch%3Amain)
[![Release](http://img.shields.io/github/release/purescript-contrib/purescript-string-parsers.svg)](https://github.com/purescript-contrib/purescript-string-parsers/releases)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-string-parsers/badge)](http://pursuit.purescript.org/packages/purescript-string-parsers)
[![Maintainer: chtenb](https://img.shields.io/badge/maintainer-chtenb-teal.svg)](http://github.com/chtenb)

A parsing library for parsing strings.

This library is a simpler, faster alternative to `purescript-parsing`, for when you know your input will be a string.

## Installation

Install `string-parsers` with [Spago](https://github.com/purescript/spago):

```sh
spago install string-parsers
```

## Quick start

The quick start hasn't been written yet (contributions are welcome!). The quick start covers a common, minimal use case for the library, whereas longer examples and tutorials are kept in the [docs directory](./docs.)

See [`test/Examples.purs`](./test/Examples.purs) for real-world examples.

## Documentation

`string-parsers` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-string-parsers).
2. Written documentation is kept in [the docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-string-parsers/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `string-parsers` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-string-parsers/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.

## Benchmark

To execute the benchmarks run the following command

```
spago run --main Bench.Main
```
