module Calc.Token where

data Token
    = TPlus
    | TMinus
    | TMultiply
    | TDivide
    | TExponentiate
    | TNumber Double
    | TOpenParenthesis
    | TCloseParenthesis
    | TEquals
    | TComma
    | TIdentifier String
    deriving (Show, Eq)
