# About

A personal project to learn Haskell and explore language implementation, including parsing, interpretation, and compiler concepts.

The project uses no libraries outside of base and Prelude. I've implemented a lexer, AST, parser, and evaluation function, along with some tests using a custom test suite.

Very little AI usage.

# Usage

Building:

```sh
$ cabal build
```

Running:

```sh
$ cabal run
```

As for how to use it, you can look at the tests for now.

Testing:

```sh
$ cabal test
```
# TODO
- Move function logic from Lexer, make tokenization dumb and treat non symbols as identifiers later defined in the parser

- Implement user implemented functions/constants

- Implement implicit multiplication

- Improve error handling (?)

- Add a proper CLI wrapper
