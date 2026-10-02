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
    | TFunction String
    deriving (Show, Eq)
