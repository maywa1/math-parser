module Calc (run) where

import Calc.Evaluator (Env, Value, evalStatement)
import Calc.Error (Error (EvaluationError))
import Calc.Lexer (tokenize)
import Calc.Parser (parseStatement)
import Data.Bifunctor (first)

run :: Env -> String -> Either Error (Value, Env)
run env input = do
  tokens <- tokenize input
  (statement, _) <- parseStatement tokens
  first (`EvaluationError` Nothing) (evalStatement env statement)
