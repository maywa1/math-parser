module Calc.Error where

import Calc.Token (Token)
import Text.Read.Lex (expect)

data Error
  = EvaluationError EvaluationError
  | LexError LexerError
  | ParseError ParserError
  deriving (Show, Eq)

data EvaluationError
  = InvalidArgumentCount Int Int -- expected, actual
  | ExpectedNumber
  | DivisionByZero
  | UndefinedVariable String
  | UndefinedFunction String
  deriving (Show, Eq)

data LexerError
  = InvalidOperator Char
  | InvalidNumber String
  deriving (Show, Eq)

data ParserError
  = DuplicateParameter String
  | SyntaxError Token
  | UnexpectedEndOfExpression
  | MissingParenthesis
  deriving (Show, Eq)


showError :: Error -> String

showError (ParseError err) =
  case err of
    DuplicateParameter param  -> "Duplicate parameter: " ++ param
    SyntaxError token         -> "Syntax error: " ++ show token
    UnexpectedEndOfExpression -> "Unexpected end of expression!"
    MissingParenthesis        -> "You forgot to close parenthesis somewhere"

showError (LexError err) =
  case err of
    InvalidOperator c -> [c] ++ " is not a valid operator!"
    InvalidNumber   n -> n ++ " is not a valid number!"

showError (EvaluationError err) =
  case err of
    UndefinedVariable str -> "Undefined variable: " ++ str
    UndefinedFunction str -> "Undefined function: " ++ str
    InvalidArgumentCount expected actual ->
      "Invalid argument count. Expected: " ++ show expected ++ "Got: " ++ show actual
    ExpectedNumber -> "Expected a number"
    DivisionByZero -> "Cannot divide by zero"
