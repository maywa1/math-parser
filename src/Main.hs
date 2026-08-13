import Data.Char (isSpace, isDigit)
import Text.Read (readMaybe)

data Operator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Exponentiation
    deriving Show

data Expr
    = Number Double
    | BinOp Operator Expr Expr
    deriving Show

data CalcError
    = InvalidOperator
    | MissingNumber
    | DivisionByZero
    deriving Show

removeWhitespace :: String -> String
removeWhitespace = filter (not . isSpace)

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
    findOperator [] = Left InvalidOperator

    findOperator (operators : rest) =
        let (rightRev, remaining) =
                span (`notElem` operators) (reverse expression)
        in case remaining of
            [] -> findOperator rest
            op : leftRev -> do
                operator <- symbolToOperator op
                Right (operator, reverse leftRev, reverse rightRev)

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
    expr <- parse (removeWhitespace expression)
    evalExpr expr

showError :: CalcError -> String
showError InvalidOperator =
    "Invalid operator in expression!"

showError MissingNumber =
    "Expected a number"

showError DivisionByZero =
    "Cannot divide by zero"


main :: IO ()
main = do
    putStrLn "Type your expression:"
    input <- getLine

    case eval input of
        Right result ->
            putStrLn ("Result is: " ++ show result)

        Left err ->
            putStrLn ("Error: " ++ showError err)
