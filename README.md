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
- Improve error handling, identify which part of the process failed using separate error types, for example ParseError, LexerError, EvalError.
- Add tests for functions and constants
- Test tokenizer, parser, eval individually (idk if this is a good idea but looks like it)
- Add a proper CLI wrapper
- Implement implicit multiplication
