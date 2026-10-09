module Calc.Token where

data SourceSpan = SourceSpan
    { spanStart :: Int
    , spanEnd   :: Int
    }
    deriving (Show, Eq)

data Token = Token
    { tokenType :: TokenType
    , tokenSpan :: SourceSpan
    }
    deriving (Show, Eq)

data TokenType
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
    | TEOF
    deriving (Show, Eq)
