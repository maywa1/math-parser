module Calc (run) where
import Calc.Evaluator (Env, Value, evalStatement, builtinEnv)
import Calc.Error (Error (EvaluationError))
import Calc.Lexer (tokenize)
import Calc.Parser (parseStatement)
import qualified Data.Map as Map
import Data.Bifunctor (first)

run :: Env -> String -> Either Error (Value, Env)
run env input = do
  tokens <- tokenize input
  (statement, _) <- parseStatement tokens
  let env' = if Map.null env then builtinEnv else env
  first (`EvaluationError` Nothing) (evalStatement env' statement)
