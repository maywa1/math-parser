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
    | TSine
    | TCosine
    | TTangent
    deriving (Show, Eq)
