import Data.Char (isSpace, isDigit)

data Operator
    = Add
    | Subtract
    | Multiply
    | Divide
    deriving Show

data Expr
    = Number Int
    | BinOp Operator Expr Expr
    deriving Show

data CalcError
    = InvalidOperator Char
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
symbolToOperator c   = Left (InvalidOperator c)


parse :: String -> Either CalcError Expr
parse expression =
    let (number, rest) = span isDigit expression
    in case (number, rest) of
        ("", _) -> Left MissingNumber
        (digits, []) -> Right (Number (read digits))
        (digits, operator : remaining) -> do
            op <- symbolToOperator operator
            right <- parse remaining
            pure (BinOp op (Number (read digits)) right)

applyOperator :: Operator -> Int -> Int -> Either CalcError Int
applyOperator Add      x y = Right (x + y)
applyOperator Subtract x y = Right (x - y)
applyOperator Multiply x y = Right (x * y)
applyOperator Divide   _ 0 = Left DivisionByZero
applyOperator Divide   x y = Right (x `div` y)


evalExpr :: Expr -> Either CalcError Int
evalExpr (Number n) = Right n

evalExpr (BinOp operator left right) = do
    x <- evalExpr left
    y <- evalExpr right
    applyOperator operator x y


eval :: String -> Either CalcError Int
eval expression = do
    expr <- parse (removeWhitespace expression)
    evalExpr expr

showError :: CalcError -> String
showError (InvalidOperator c) =
    "Character '" ++ [c] ++ "' is not a valid operator"

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
