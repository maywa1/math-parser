# About

Easy to use repl and haskell library for interpreting math from text.

A personal project to learn Haskell and explore language implementation, including parsing, interpretation, and compiler concepts.

The project uses no libraries outside of the ones included in ghc. I've implemented a lexer, AST, parser, and evaluator, along with some tests using a custom test suite (not much thought was put into it tbh :p).

Very little AI usage (mostly for tests).

# Features

- __Arithmetic operations:__ addition, subtraction, multiplication, division, exponentiation, and unary negation.
- __Expression evaluation:__ operator precedence, associativity, parentheses, nested expressions, and decimal numbers.
- __Mathematical functions:__ built-in functions `sin`, `cos`, `tan`, `sqrt`, `abs`, `negate`, `floor`, `ceil`, `round`, `ln`, `log`, `exp`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`
- __Mathematical constants:__ pi and e.
- __Variables:__ variable definitions, persistent evaluation environments, and parameter shadowing.
- __User-defined functions:__ single and multi-parameter functions, function redefinition, nested calls, and calls between functions.
- __Higher-order functions:__ passing functions as arguments, repeated function application, and function composition.
- __Error handling:__ lexical, parsing, and evaluation errors with specific error types.

# Usage

Building:

```sh
$ cabal build
```

Running:

```sh
$ cabal run
```
Testing:

```sh
$ cabal test
```
# Examples

For how to use it, you can look at the tests for now, and run them since not all of them pass yet.

# TODO
- Add accurate values, allowing results to be in fractions and sqrt.
- Polish REPL (add autocomplete to functions and constants, colors in error messages, maybe more) and create a cli wrapper for the library
- Loading environment from a file and into a file
- Implement implicit multiplication
- Add proper documentation with haddock (WIP, still don't know how much coverage I have to get), maybe publish in hackage in the future?
- Test tokenizer, parser, eval individually (idk if this is a good idea but looks like it)
