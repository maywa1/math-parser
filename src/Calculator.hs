module Calculator where

import Data.Char (isSpace, isDigit)
import Text.Read (readMaybe)
import Data.List (isInfixOf)

data Token
    = TPlus
    | TMinus
    | TMultiply
    | TDivide
    | TExponentiate
    | TNumber Double
    | TOpenParenthesis
    | TCloseParenthesis
    deriving (Show, Eq)

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

data CalcError
    = InvalidOperator Char
    | InvalidNumber String
    | MissingNumber
    | DivisionByZero
    | SyntaxError Token
    | MissingParenthesis
    | UnexpectedEndOfExpression
    deriving (Show, Eq)

tokenize :: String -> Either CalcError [Token]
tokenize = go
  where
    go :: String -> Either CalcError [Token]
    go input =
      case input of
        c : rest
          | isSpace c -> go rest
        _ ->
          let (whole, rest) = span isDigit input
          in case rest of
               '.' : decimalRest ->
                 let (fraction, remaining) = span isDigit decimalRest
                     number = whole ++ "." ++ fraction
                 in if null whole || null fraction
                      then Left (InvalidNumber number)
                      else prepend (TNumber (read number)) remaining

               _ | not (null whole) ->
                   prepend (TNumber (read whole)) rest

               '(' : remaining -> prepend TOpenParenthesis remaining
               ')' : remaining -> prepend TCloseParenthesis remaining
               '+' : remaining -> prepend TPlus remaining
               '-' : remaining -> prepend TMinus remaining
               '/' : remaining -> prepend TDivide remaining
               '*' : remaining -> prepend TMultiply remaining
               '^' : remaining -> prepend TExponentiate remaining
               symbol : _       -> Left (InvalidOperator symbol)
               []               -> Right []

    prepend :: Token -> String -> Either CalcError [Token]
    prepend token rest =
      (token :) <$> go rest

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
