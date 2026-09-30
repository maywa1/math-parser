module Calc.Error where

import Calc.Token (Token)

data CalcError
    = InvalidOperator Char
    | InvalidNumber String
    | MissingNumber
    | DivisionByZero
    | SyntaxError Token
    | MissingParenthesis
    | UnexpectedEndOfExpression
    deriving (Show, Eq)

showError :: CalcError -> String
showError (InvalidOperator symbol) =
    [symbol] ++ " is not a valid operator!"

showError (InvalidNumber number) =
    number ++ " is not a valid number!"

showError MissingParenthesis  =
    "You forgot to close parenthesis somewhere"

showError UnexpectedEndOfExpression  =
    "The expression ended unexpectedly"

showError MissingNumber =
    "Expected a number"

showError DivisionByZero =
    "Cannot divide by zero"

showError (SyntaxError token) =
    "Syntax error: " ++ show token
