module Calc.Error where

import Calc.Token (Token)
import Text.Read.Lex (expect)

data CalcError
    = InvalidOperator Char
    | InvalidNumber String
    | UndefinedVariable String
    | UndefinedFunction String
    | ExpectedNumber
    | DivisionByZero
    | SyntaxError Token
    | MissingParenthesis
    | UnexpectedEndOfExpression
    | DuplicateParameter String
    | InvalidArgumentCount Int Int -- expected, actual
    deriving (Show, Eq)

showError :: CalcError -> String
showError (InvalidOperator symbol) =
    [symbol] ++ " is not a valid operator!"

showError (InvalidNumber number) =
    number ++ " is not a valid number!"

showError (UndefinedVariable str) =
    "Undefined variable: " ++ str

showError (UndefinedFunction str) =
    "Undefined function: " ++ str

showError MissingParenthesis  =
    "You forgot to close parenthesis somewhere"

showError (InvalidArgumentCount expected actual) =
    "Invalid argument count. Expected: " ++ show expected ++ "Got: " ++ show actual

showError ExpectedNumber =
    "Expected a number"

showError DivisionByZero =
    "Cannot divide by zero"

showError (SyntaxError token) =
    "Syntax error: " ++ show token

showError UnexpectedEndOfExpression =
    "Unexpected end of expression!"

showError (DuplicateParameter p)=
    "Duplicate parameter: " ++ p
