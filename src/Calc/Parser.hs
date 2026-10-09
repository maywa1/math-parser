module Calc.Parser where

import Calc.AST
import Calc.Error (Error (..), ParserError (..))
import Calc.Token (SourceSpan (..), Token (..), TokenType (..))
import Data.Bifunctor (first)

type Parser a = [Token] -> Either Error (a, [Token])

-- operator tables only care about the kind of token, not its position
type OpTable = TokenType -> Maybe (Expr -> Expr -> Expr)

-- error helpers

-- fail at the first remaining token
failAt :: ParserError -> [Token] -> Either Error a
failAt err (t : _) = Left (ParseError err (tokenSpan t))
failAt err [] = Left (ParseError err (SourceSpan 0 0)) -- unreachable if the lexer always appends TEOF

-- at EOF report the given error, anywhere else it's a syntax error
unexpectedOr :: ParserError -> [Token] -> Either Error a
unexpectedOr err ts@(Token TEOF _ : _) = failAt err ts
unexpectedOr _ ts = failAt SyntaxError ts

unexpected :: [Token] -> Either Error a
unexpected = unexpectedOr UnexpectedEndOfExpression

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

unaryOp :: TokenType -> Maybe UnaryOperator
unaryOp TPlus = Just Positive
unaryOp TMinus = Just Negative
unaryOp _ = Nothing

-- combinators

-- left associative chain: a - b - c == (a - b) - c
chainl1 :: Parser Expr -> OpTable -> Parser Expr
chainl1 p opTable tokens = do
  (firstExpr, rest) <- p tokens
  loop firstExpr rest
  where
    loop acc (t : ts)
      | Just op <- opTable (tokenType t) = do
          (right, rest') <- p ts
          loop (op acc right) rest'
    loop acc ts = pure (acc, ts)

-- grammar

parseStatement :: Parser Statement
parseStatement tokens = do
  (stmt, rest) <-
    if TEquals `elem` map tokenType tokens
      then parseDefinition tokens
      else first Expression <$> parseExpression tokens
  case rest of
    [Token TEOF _] -> pure (stmt, [])
    _ -> failAt SyntaxError rest

parseDefinition :: Parser Statement
parseDefinition (Token (TIdentifier name) _ : Token TOpenParenthesis _ : rest) = do
  (params, rest') <- parseParams rest
  case rest' of
    Token TEquals _ : body -> do
      (expr, rest'') <- parseExpression body
      pure (FunctionDefinition name params expr, rest'')
    _ -> unexpected rest'
parseDefinition (Token (TIdentifier name) _ : Token TEquals _ : rest) = do
  (expr, rest') <- parseExpression rest
  pure (ConstantDefinition name expr, rest')
parseDefinition tokens = unexpected tokens

parseParams :: Parser [String]
parseParams = go []
  where
    go prevParams tokens =
      case tokens of
        nameTok@(Token (TIdentifier name) _) : Token TComma _ : rest -> do
          params <- addParam nameTok name prevParams
          go params rest
        nameTok@(Token (TIdentifier name) _) : Token TCloseParenthesis _ : rest -> do
          params <- addParam nameTok name prevParams
          pure (params, rest)
        Token (TIdentifier _) _ : rest -> unexpectedOr MissingParenthesis rest
        _ -> unexpectedOr MissingParenthesis tokens

    addParam tok name prevParams
      | name `elem` prevParams = failAt (DuplicateParameter name) [tok]
      | otherwise = Right (prevParams ++ [name])

parseExpression :: Parser Expr
parseExpression = chainl1 parseTerm addOp

parseTerm :: Parser Expr
parseTerm = chainl1 parseUnary mulOp

parseUnary :: Parser Expr
parseUnary (t : rest)
  | Just op <- unaryOp (tokenType t) = do
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
      | Just op <- powOp (tokenType t) -> do
          (expo, rest') <- parseUnary ts
          pure (op base expo, rest')
    _ -> pure (base, rest)

parseFactor :: Parser Expr
parseFactor tokens =
  case tokens of
    Token (TNumber n) _ : rest -> pure (Number n, rest)
    Token (TIdentifier name) _ : rest -> parseApplication name rest
    Token TOpenParenthesis _ : rest -> parseParenthesized rest
    _ -> unexpected tokens

parseApplication :: String -> Parser Expr
parseApplication f tokens =
  case tokens of
    Token TOpenParenthesis _ : rest ->
      first (ApplyFunction f) <$> parseParenthesizedFunArgs rest
    Token (TNumber n) _ : rest -> Right (ApplyFunction f [Number n], rest)
    Token (TIdentifier _) _ : _ -> failAt SyntaxError tokens
    _ -> Right (Variable f, tokens)

parseParenthesizedFunArgs :: Parser [Expr]
parseParenthesizedFunArgs tokens = do
  (expr, rest) <- parseExpression tokens
  case rest of
    Token TCloseParenthesis _ : rest' -> pure ([expr], rest')
    Token TComma _ : rest' -> do
      (nextExprs, rest'') <- parseParenthesizedFunArgs rest'
      pure (expr : nextExprs, rest'')
    _ -> unexpectedOr MissingParenthesis rest

parseParenthesized :: Parser Expr
parseParenthesized tokens = do
  (expr, rest) <- parseExpression tokens
  case rest of
    Token TCloseParenthesis _ : rest' -> pure (expr, rest')
    _ -> unexpectedOr MissingParenthesis rest
