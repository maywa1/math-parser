module Calc.Evaluator(interpreter) where

import Calc.Error (CalcError(..))
import Calc.Parser
  ( Operator(..)
  , Expr(..)
  , UnaryOperator(..)
  , Statement(..), parseStatement
  )
import qualified Data.Map as Map
import Calc.Lexer (tokenize)

type Env = Map.Map String Value

data Value
  = VNumber Double
  | VFunction [String] Expr
  | VBuiltinFunction ([Double] -> Either CalcError Double)

instance Show Value where
  show (VNumber n) = show n
  show (VFunction args _) = "<function(" ++ unwords args ++ ")>"
  show (VBuiltinFunction _) = "<builtin function>"

builtinEnv :: Env
builtinEnv =
  Map.fromList
    [ ("sin", VBuiltinFunction (runFunction (\[x] -> sin x) 1))
    , ("cos", VBuiltinFunction (runFunction (\[x] -> cos x) 1))
    , ("tan", VBuiltinFunction (runFunction (\[x] -> tan x) 1))
    , ("pi" , VNumber pi)
    , ("e" , VNumber (exp 1))
    ]
    where
      runFunction :: ([Double] -> Double) -> Int -> [Double] -> Either CalcError Double
      runFunction func expected args =
        if length args == expected
          then Right (func args)
          else Left (InvalidArgumentCount (length args) expected)

applyOperator :: Operator -> Double -> Double -> Either CalcError Double
applyOperator Add            x y = Right (x + y)
applyOperator Subtract       x y = Right (x - y)
applyOperator Multiply       x y = Right (x * y)
applyOperator Exponentiation x y = Right (x ** y)
applyOperator Divide         _ 0 = Left DivisionByZero
applyOperator Divide         x y = Right (x / y)

eval :: Env -> Expr -> Either CalcError Value
eval env (Number n) = Right (VNumber n)

eval env (UnaryOp Negative expr) = do
  value <- eval env expr

  case value of
    VNumber x -> pure (VNumber (-x))
    _         -> Left ExpectedNumber

eval env (ApplyFunction f expressions) =
  case Map.lookup f env of
    Just (VFunction args body) -> do
      values <- mapM (eval env) expressions

      let tempEnv = Map.union (Map.fromList (zip args values)) env

      eval tempEnv body

    Just (VBuiltinFunction function) -> do
      values <- mapM (eval env) expressions
      numbers <- mapM unwrapNumber values
      result <- function numbers
      pure (VNumber result)

    Nothing -> Left (UndefinedFunction f)

  where
    unwrapNumber :: Value -> Either CalcError Double
    unwrapNumber (VNumber n) = pure n
    unwrapNumber _ = Left ExpectedNumber


eval env (Variable x) =
  case Map.lookup x env of
    Just value -> pure value
    Nothing    -> Left (UndefinedVariable x)

eval env (UnaryOp Positive expr) = do
  x <- eval env expr
  pure x

eval env (BinOp operator left right) = do
  leftValue <- eval env left
  rightValue <- eval env right

  case (leftValue, rightValue) of
    (VNumber x, VNumber y) -> do
      result <- applyOperator operator x y
      pure (VNumber result)

    _ -> Left ExpectedNumber

evalStatement :: Env -> Statement -> Either CalcError (Value, Env)
evalStatement env statement =
  case statement of
    Expression expr -> do
      value <- eval env expr
      pure (value, env)

    FunctionDefinition name args body ->
      let function = VFunction args body
          env' = Map.insert name function env
      in pure (function, env')

    ConstantDefinition name body -> do
      value <- eval env body
      case value of
        VNumber n ->
          let env' = Map.insert name (VNumber n) env
          in Right (VNumber n, env')
        _         -> Left ExpectedNumber

interpreter :: Env -> String -> Either CalcError (Value, Env)
interpreter env input = do
  tokens <- tokenize input
  (statement, _) <- parseStatement tokens
  if Map.null env then do
    result <- (evalStatement builtinEnv statement)
    pure result
  else do
    result <- (evalStatement env statement)
    pure result
