module Calc.Parser where

import Calc.Error (CalcError (..))
import Calc.Token (Token (..))

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

data Expr
  = Number Double
  | UnaryOp UnaryOperator Expr
  | BinOp Operator Expr Expr
  | ApplyFunction String Expr
  deriving (Show, Eq)

type Parser = [Token] -> Either CalcError (Expr, [Token])

type OpTable = Token -> Maybe (Expr -> Expr -> Expr)

chainl1 :: Parser -> OpTable -> Parser
chainl1 p opTable tokens = do
  (first, remaining) <- p tokens
  loop first remaining
  where
    loop acc (t : rest)
      | Just op <- opTable t = do
          (right, rest') <- p rest
          loop (op acc right) rest'
    loop acc remaining = pure (acc, remaining)

chainr1 :: Parser -> OpTable -> Parser
chainr1 p opTable tokens = do
  (left, remaining) <- p tokens
  case remaining of
    t : rest
      | Just op <- opTable t -> do
          (right, rest') <- chainr1 p opTable rest
          pure (op left right, rest')
    _ -> pure (left, remaining)

addOp :: OpTable
addOp TPlus = Just (BinOp Add)
addOp TMinus = Just (BinOp Subtract)
addOp _ = Nothing

mulOp :: OpTable
mulOp TMultiply = Just (BinOp Multiply)
mulOp TDivide = Just (BinOp Divide)
mulOp _ = Nothing

powOp :: OpTable
powOp TExponentiate = Just (BinOp Exponentiation)
powOp _ = Nothing

parseExpression :: Parser
parseExpression = chainl1 parseTerm addOp

parseTerm :: Parser
parseTerm = chainl1 parseUnary mulOp

parseUnary :: Parser
parseUnary (TMinus : rest) = do
  (expr, rest') <- parseUnary rest
  pure (UnaryOp Negative expr, rest')
parseUnary (TPlus : rest) = do
  (expr, rest') <- parseUnary rest
  pure (UnaryOp Positive expr, rest')
parseUnary tokens = parsePower tokens

parsePower :: Parser
parsePower = chainr1 parseFactor powOp

parseFactor :: Parser
parseFactor tokens =
  case tokens of
    TNumber n : remaining ->
      pure (Number n, remaining)

    TOpenParenthesis : remaining ->
      parseParenthesized Nothing remaining

    TFunction f : remaining ->
      parseFunctionApplication f remaining

    token : _ -> Left (SyntaxError token)
    [] -> Left UnexpectedEndOfExpression

parseFunctionApplication :: String -> [Token] -> Either CalcError (Expr, [Token])
parseFunctionApplication f tokens =
  case tokens of
    TOpenParenthesis : remaining ->
      parseParenthesized (Just $ ApplyFunction f) remaining

    TNumber n : remaining ->
      pure (ApplyFunction f (Number n), remaining)

    token : _ -> Left (SyntaxError token)
    [] -> Left UnexpectedEndOfExpression

parseParenthesized :: Maybe (Expr -> Expr) -> [Token] -> Either CalcError (Expr, [Token])
parseParenthesized exprConstructor remaining = do
  (expr, rest) <- parseExpression remaining

  let builtExpr = case exprConstructor of
        Nothing -> expr
        Just constructor -> constructor expr

  case rest of
    TCloseParenthesis : rest' -> pure (builtExpr, rest')
    token : _                 -> Left (SyntaxError token)
    []                        -> Left MissingParenthesis
