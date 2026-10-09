module Calc.AST
  ( Operator (..)
  , UnaryOperator (..)
  , Statement (..)
  , Expr (..)
  ) where

data Operator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Exponentiation
  deriving (Show, Eq)

data UnaryOperator
  = Positive
  | Negative
  deriving (Show, Eq)

data Statement
  = Expression Expr
  | FunctionDefinition String [String] Expr
  | ConstantDefinition String Expr
  deriving (Show, Eq)

data Expr
  = Number Double
  | Variable String
  | UnaryOp UnaryOperator Expr
  | BinOp Operator Expr Expr
  | ApplyFunction String [Expr]
  deriving (Show, Eq)
