module Calc.Evaluator where

import Calc.Error (CalcError(..))
import Calc.Parser (Operator(..), Expr(..), UnaryOperator(..), parseExpression)
import Calc.Lexer (tokenize)

applyOperator :: Operator -> Double -> Double -> Either CalcError Double
applyOperator Add            x y = Right (x + y)
applyOperator Subtract       x y = Right (x - y)
applyOperator Multiply       x y = Right (x * y)
applyOperator Exponentiation x y = Right (x ** y)
applyOperator Divide         _ 0 = Left DivisionByZero
applyOperator Divide         x y = Right (x / y)


evalExpr :: Expr -> Either CalcError Double
evalExpr (Number n) = Right n

evalExpr (UnaryOp Negative expr) = do
    x <- evalExpr expr
    pure (-x)

evalExpr (UnaryOp Positive expr) = do
    x <- evalExpr expr
    pure x

evalExpr (BinOp operator left right) = do
    x <- evalExpr left
    y <- evalExpr right
    applyOperator operator x y


eval :: String -> Either CalcError Double
eval input = do
    tokens <- tokenize input
    (expr, _) <- parseExpression tokens
    evalExpr expr
