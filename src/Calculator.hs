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
               '/' : remaining -> prepend TDivide remaining
               '*' : remaining -> prepend TMultiply remaining
               '-' : remaining -> prepend TMinus remaining
               symbol : _       -> Left (InvalidOperator symbol)
               []               -> Right []

    prepend :: Token -> String -> Either CalcError [Token]
    prepend token rest =
      (token :) <$> go rest


splitLowestPriorityOperator :: String -> Either CalcError (Operator, String, String)
splitLowestPriorityOperator expression =
    findOperator ["+-", "*/", "^"]
    where
    splitOnElemInString :: String -> String -> (String, String)
    splitOnElemInString string elem =
        span (`notElem` elem) string

    findOperator [] = Left InvalidOperator

    findOperator ("^" : rest) =
        let (left, remaining) = splitOnElemInString expression "^"
        in case remaining of
            [] -> findOperator rest
            _ : right -> Right (Exponentiation, left, right)

    findOperator (operators : rest) =
        let (rightRev, remaining) = splitOnElemInString (reverse expression) operators
        in case remaining of
            [] -> findOperator rest
            op : leftRev ->
                if leftRev == "" then
                    findOperator rest
                else if op == '-' && head leftRev == '/'
                    || op == '-' && head leftRev == '*' then
                    let (remainingRightRev, remainingLeftRev) = splitOnElemInString leftRev operators
                    in if remainingLeftRev == "" then
                        findOperator rest
                    else do
                        operator <- symbolToOperator op
                        pure (operator, reverse remainingLeftRev, (reverse (remainingRightRev ++ rightRev)))

                else do
                    operator <- symbolToOperator op
                    pure (operator, reverse leftRev, reverse rightRev)

parse :: String -> Either CalcError Expr
parse expression =
    case readMaybe expression of
        Just number -> Right (Number number)
        Nothing -> do
            (op, left, right) <- splitLowestPriorityOperator expression
            leftExpr <- parse left
            rightExpr <- parse right
            pure (BinOp op leftExpr rightExpr)

applyOperator :: Operator -> Double -> Double -> Either CalcError Double
applyOperator Add            x y = Right (x + y)
applyOperator Subtract       x y = Right (x - y)
applyOperator Multiply       x y = Right (x * y)
applyOperator Exponentiation x y = Right (x ** y)
applyOperator Divide         _ 0 = Left DivisionByZero
applyOperator Divide         x y = Right (x / y)


evalExpr :: Expr -> Either CalcError Double
evalExpr (Number n) = Right n

evalExpr (BinOp operator left right) = do
    x <- evalExpr left
    y <- evalExpr right
    applyOperator operator x y


eval :: String -> Either CalcError Double
eval expression = do
    expr <- parse (tokenize expression)
    evalExpr expr

showError :: CalcError -> String
showError (InvalidOperator symbol) =
    [symbol] ++ " is not a valid operator!"

showError (InvalidNumber number) =
    number ++ " is not a valid number!"

showError MissingNumber =
    "Expected a number"

showError DivisionByZero =
    "Cannot divide by zero"
