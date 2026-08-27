module Calculator where

import Data.Char (isSpace, isDigit)
import Text.Read (readMaybe)
import Data.List (isInfixOf)

data Operator
data Operator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Exponentiation
    deriving (Show, Eq)

data Expr
    = Number Double
    | BinOp Operator Expr Expr
    deriving (Show, Eq)

data CalcError
    = InvalidOperator
    | MissingNumber
    | DivisionByZero
    deriving (Show, Eq)

cleanExpresssion :: String -> String
cleanExpresssion expression =
    removePlusAtStart $ simplifySigns $ cleanWhiteSpace expression
    where
        removePlusAtStart :: String -> String
        removePlusAtStart str =
            if head str == '+' then
                tail str
            else
                str


        cleanWhiteSpace :: String -> String
        cleanWhiteSpace = filter (not . isSpace)

        simplifySigns :: String -> String
        simplifySigns expression
            | "--"  `isInfixOf`  expression  = simplifySigns $ replace "--" "+" expression
            | "+-"  `isInfixOf`  expression  = simplifySigns $ replace "+-" "-" expression
            | "-+"  `isInfixOf`  expression  = simplifySigns $ replace "-+" "-" expression
            | "++"  `isInfixOf`  expression  = simplifySigns $ replace "++" "+" expression
            | otherwise = expression

        -- I wanted to not use any package so I will stick to strings and have my own fuction
        replace :: Eq a => [a] -> [a] -> [a] -> [a]
        replace old new = go
          where
            go [] = []
            go xs
              | old `isPrefixOf` xs = new ++ go (drop (length old) xs)
              | otherwise           = head xs : go (tail xs)

            isPrefixOf [] _ = True
            isPrefixOf _ [] = False
            isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys


symbolToOperator :: Char -> Either CalcError Operator
symbolToOperator '+' = Right Add
symbolToOperator '-' = Right Subtract
symbolToOperator '*' = Right Multiply
symbolToOperator '/' = Right Divide
symbolToOperator '^' = Right Exponentiation
symbolToOperator c   = Left InvalidOperator

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
    expr <- parse (cleanExpresssion expression)
    evalExpr expr

showError :: CalcError -> String
showError InvalidOperator =
    "Invalid operator in expression!"

showError MissingNumber =
    "Expected a number"

showError DivisionByZero =
    "Cannot divide by zero"
