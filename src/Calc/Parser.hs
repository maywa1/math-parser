module Calc.Parser where

import Calc.Token (Token(..))
import Calc.Error (CalcError(..))

data Operator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Exponentiation
    deriving (Show, Eq)

data Expr
    = Number Double
    | UnaryOp UnaryOperator Expr
    | BinOp Operator Expr Expr
    deriving (Show, Eq)

data UnaryOperator
    = Positive
    | Negative
    deriving (Show, Eq)

parseExpression :: [Token] -> Either CalcError (Expr, [Token])
parseExpression tokens = do
    (left, remaining) <- parseTerm tokens

    case remaining of
        TPlus : rest -> do
            (right, rest') <- parseExpression rest
            pure (BinOp Add left right, rest')

        TMinus : rest -> do
            (right, rest') <- parseExpression rest
            pure (BinOp Subtract left right, rest')

        _ -> pure (left, remaining)


parseTerm :: [Token] -> Either CalcError (Expr, [Token])
parseTerm tokens = do
    (left, remaining) <- parseUnary tokens

    case remaining of
        TMultiply : rest -> do
            (right, rest') <- parseTerm rest
            pure (BinOp Multiply left right, rest')

        TDivide : rest -> do
            (right, rest') <- parseTerm rest
            pure (BinOp Divide left right, rest')

        _ -> pure (left, remaining)

parseUnary :: [Token] -> Either CalcError (Expr, [Token])
parseUnary tokens = do
    case tokens of
        TMinus : remaining -> do
            (expr, rest) <- parseUnary remaining
            pure (UnaryOp Negative expr, rest)

        TPlus : remaining -> do
            (expr, rest) <- parseUnary remaining
            pure (UnaryOp Positive expr, rest)

        _ -> parsePower tokens


parsePower :: [Token] -> Either CalcError (Expr, [Token])
parsePower tokens = do
    (left, remaining) <- parseFactor tokens

    case remaining of
        TExponentiate : rest -> do
            (right, rest') <- parsePower rest
            pure (BinOp Exponentiation left right, rest')
        _ -> pure (left, remaining)


parseFactor :: [Token] -> Either CalcError (Expr, [Token])
parseFactor tokens =
    case tokens of
        TNumber n : remaining ->
            pure (Number n, remaining)


        TOpenParenthesis : remaining -> do
            (expr, rest) <- parseExpression remaining
            case rest of
                TCloseParenthesis : rest' ->
                    pure (expr, rest')
                token : _ ->
                    Left (SyntaxError token)
                [] ->
                    Left MissingParenthesis

        token : _ ->
            Left (SyntaxError token)

        [] ->
            Left UnexpectedEndOfExpression

