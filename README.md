# About

A personal project to learn Haskell and explore language implementation, including parsing, interpretation, and compiler concepts.

The project uses no libraries outside of base, containers, and Prelude. I've implemented a lexer, AST, parser, and evaluator, along with some tests using a custom test suite (a bit simple though).

Very little AI usage (mostly for tests).

# Usage

Building:

```sh
$ cabal build
```

Running:

```sh
$ cabal run
```

As for how to use it, you can look at the tests for now, and run them since not all of them pass yet.

Testing:

```sh
$ cabal test
```
# TODO
- Add proper documentation with haddock (WIP, still don't know how much coverage I have to get), maybe publish in hackage in the future?
- Test tokenizer, parser, eval individually (idk if this is a good idea but looks like it)
- Add a proper CLI wrapper
- Implement implicit multiplication
