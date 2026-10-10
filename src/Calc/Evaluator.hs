module Calc.Evaluator(Value (..), Env, evalStatement) where

import Calc.Error (EvaluationError (ExpectedNumber, UndefinedVariable, UndefinedFunction, DivisionByZero, InvalidArgumentCount), Error(..))
import Calc.Parser(parseStatement)
import Calc.AST
import qualified Data.Map as Map
import Calc.Lexer (tokenize)
import Data.Bifunctor (first)

type Env = Map.Map String Value

data Value
  = VNumber Double
  | VFunction [String] Expr
  | VBuiltinFunction ([Double] -> Either EvaluationError Double)

instance Show Value where
  show (VNumber n) = show n
  show (VFunction args _) = "<function(" ++ unwords args ++ ")>"
  show (VBuiltinFunction _) = "<builtin function>"

builtinEnv :: Env
builtinEnv =
  Map.fromList
    [ ("sin"  , VBuiltinFunction (runFunction (\[x] -> sin x) 1))
    , ("cos"  , VBuiltinFunction (runFunction (\[x] -> cos x) 1))
    , ("tan"  , VBuiltinFunction (runFunction (\[x] -> tan x) 1))
    , ("sqrt" , VBuiltinFunction (runFunction (\[x] -> sqrt x) 1))
    , ("abs"   , VBuiltinFunction (runFunction (\[x] -> abs x) 1))
    , ("negate", VBuiltinFunction (runFunction (\[x] -> negate x) 1))
    , ("floor" , VBuiltinFunction (runFunction (\[x] -> fromIntegral (floor x :: Integer)) 1))
    , ("ceil"  , VBuiltinFunction (runFunction (\[x] -> fromIntegral (ceiling x :: Integer)) 1))
    , ("round" , VBuiltinFunction (runFunction (\[x] -> fromIntegral (round x :: Integer)) 1))
    , ("ln"   , VBuiltinFunction (runFunction (\[x] -> log x) 1))
    , ("log" , VBuiltinFunction (runFunction (\[x, y] -> logBase x y) 2))
    , ("exp"   , VBuiltinFunction (runFunction (\[x] -> exp x) 1))
    , ("asin"  , VBuiltinFunction (runFunction (\[x] -> asin x) 1))
    , ("acos"  , VBuiltinFunction (runFunction (\[x] -> acos x) 1))
    , ("atan"  , VBuiltinFunction (runFunction (\[x] -> atan x) 1))
    , ("sinh"  , VBuiltinFunction (runFunction (\[x] -> sinh x) 1))
    , ("cosh"  , VBuiltinFunction (runFunction (\[x] -> cosh x) 1))
    , ("tanh"  , VBuiltinFunction (runFunction (\[x] -> tanh x) 1))
    , ("pi"   , VNumber pi)
    , ("e"    , VNumber (exp 1))
    ]
    where
      runFunction :: ([Double] -> Double) -> Int -> [Double] -> Either EvaluationError Double
      runFunction func expected args =
        if length args == expected
          then Right (func args)
          else Left (InvalidArgumentCount expected (length args))

applyOperator :: Operator -> Double -> Double -> Either EvaluationError Double
applyOperator Add            x y = Right (x + y)
applyOperator Subtract       x y = Right (x - y)
applyOperator Multiply       x y = Right (x * y)
applyOperator Exponentiation x y = Right (x ** y)
applyOperator Divide         _ 0 = Left DivisionByZero
applyOperator Divide         x y = Right (x / y)

eval :: Env -> Expr -> Either EvaluationError Value
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
      let argCount = length values
      let expectedArgCount = length args

      if argCount == expectedArgCount then do
        let tempEnv = Map.union (Map.fromList (zip args values)) env
        eval tempEnv body
      else Left (InvalidArgumentCount expectedArgCount argCount)

    Just (VBuiltinFunction function) -> do
      values <- mapM (eval env) expressions
      numbers <- mapM unwrapNumber values
      result <- function numbers
      pure (VNumber result)

    Nothing -> Left (UndefinedFunction f)

  where
    unwrapNumber :: Value -> Either EvaluationError Double
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

evalStatement :: Env -> Statement -> Either EvaluationError (Value, Env)
evalStatement userEnv statement =
  case statement of
    Expression expr -> do
      value <- eval fullEnv expr
      pure (value, userEnv)

    FunctionDefinition name args body ->
      let function = VFunction args body
      in pure (function, Map.insert name function userEnv)

    ConstantDefinition name body -> do
      value <- eval fullEnv body
      case value of
        VNumber _ -> Right (value, Map.insert name value userEnv)
        _         -> Left ExpectedNumber
  where
    -- Left-biased union: user definitions shadow builtins.
    fullEnv = Map.union userEnv builtinEnv
