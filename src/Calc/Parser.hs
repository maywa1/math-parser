module Calc.Parser where

import Calc.Error (ParserError (..))
import Calc.Token (Token (..))
import Data.Bifunctor (first)

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

type Parser a = [Token] -> Either ParserError (a, [Token])

type OpTable = Token -> Maybe (Expr -> Expr -> Expr)

-- operator tables

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

unaryOp :: Token -> Maybe UnaryOperator
unaryOp TPlus = Just Positive
unaryOp TMinus = Just Negative
unaryOp _ = Nothing

-- combinators

-- | left associative chain: a - b - c == (a - b) - c
chainl1 :: Parser Expr -> OpTable -> Parser Expr
chainl1 p opTable tokens = do
  (firstExpr, rest) <- p tokens
  loop firstExpr rest
  where
    loop acc (t : ts)
      | Just op <- opTable t = do
          (right, rest') <- p ts
          loop (op acc right) rest'
    loop acc ts = pure (acc, ts)

-- grammar

parseStatement :: Parser Statement
parseStatement tokens = do
  (stmt, rest) <-
    if TEquals `elem` tokens
      then parseDefinition tokens
      else first Expression <$> parseExpression tokens
  case rest of
    [] -> pure (stmt, [])
    token : _ -> Left (SyntaxError token)

parseDefinition :: Parser Statement
parseDefinition (TIdentifier name : TOpenParenthesis : rest) = do
  (params, rest') <- parseParams rest
  case rest' of
    TEquals : body -> do
      (expr, rest'') <- parseExpression body
      pure (FunctionDefinition name params expr, rest'')
    token : _ -> Left (SyntaxError token)
    [] -> Left UnexpectedEndOfExpression
parseDefinition (TIdentifier name : TEquals : rest) = do
  (expr, rest') <- parseExpression rest
  pure (ConstantDefinition name expr, rest')
parseDefinition (token : _) = Left (SyntaxError token)
parseDefinition [] = Left UnexpectedEndOfExpression

parseParams :: Parser [String]
parseParams = go []
  where
    go prevParams tokens =
      case tokens of
        TIdentifier name : TComma : rest -> do
          params <- addParam name prevParams
          go params rest
        TIdentifier name : TCloseParenthesis : rest -> do
          params <- addParam name prevParams
          pure (params, rest)
        TIdentifier _ : t : _ -> Left (SyntaxError t)
        [TIdentifier _] -> Left MissingParenthesis
        t : _ -> Left (SyntaxError t)
        [] -> Left MissingParenthesis

    addParam name prevParams
      | name `elem` prevParams = Left (DuplicateParameter name)
      | otherwise = Right (prevParams ++ [name])


parseExpression :: Parser Expr
parseExpression = chainl1 parseTerm addOp

parseTerm :: Parser Expr
parseTerm = chainl1 parseUnary mulOp

parseUnary :: Parser Expr
parseUnary (t : rest)
  | Just op <- unaryOp t = do
      (expr, rest') <- parseUnary rest
      pure (UnaryOp op expr, rest')
parseUnary tokens = parsePower tokens

-- | right associative, and the exponent may carry a unary sign:
--   2 ^ 3 ^ 2 == 2 ^ (3 ^ 2),  2 ^ -3 == 2 ^ (-3),  -2 ^ 2 == -(2 ^ 2)
parsePower :: Parser Expr
parsePower tokens = do
  (base, rest) <- parseFactor tokens
  case rest of
    t : ts
      | Just op <- powOp t -> do
          (expo, rest') <- parseUnary ts
          pure (op base expo, rest')
    _ -> pure (base, rest)

parseFactor :: Parser Expr
parseFactor tokens =
  case tokens of
    TNumber n : rest -> pure (Number n, rest)
    TIdentifier name : rest -> parseApplication name rest
    TOpenParenthesis : rest -> parseParenthesized rest
    token : _ -> Left (SyntaxError token)
    [] -> Left UnexpectedEndOfExpression

parseApplication :: String -> Parser Expr
parseApplication f tokens =
  case tokens of
    TOpenParenthesis : rest -> first (ApplyFunction f) <$> parseParenthesizedFunArgs rest
    TNumber n : rest -> Right (ApplyFunction f [Number n], rest)
    TIdentifier i : _ -> Left (SyntaxError (TIdentifier i))
    _ -> Right (Variable f, tokens)

parseParenthesizedFunArgs :: Parser [Expr]
parseParenthesizedFunArgs tokens = do
  (expr, rest) <- parseExpression tokens
  case rest of
    TCloseParenthesis : rest' -> pure ([expr], rest')
    TComma : rest' -> do
      (nextExprs, rest'') <- parseParenthesizedFunArgs rest'
      pure (expr : nextExprs, rest'')
    token : _ -> Left (SyntaxError token)
    [] -> Left MissingParenthesis

parseParenthesized :: Parser Expr
parseParenthesized tokens = do
  (expr, rest) <- parseExpression tokens
  case rest of
    TCloseParenthesis : rest' -> pure (expr, rest')
    token : _ -> Left (SyntaxError token)
    [] -> Left MissingParenthesis
