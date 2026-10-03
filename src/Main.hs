module Main where

import Calc.Evaluator (interpreter)
import Calc.Error (showError)
import qualified Data.Map as Map

main :: IO ()
main =
    loop Map.empty
  where
    loop env = do
      putStrLn "> "
      input <- getLine

      case interpreter env input of
        Right (result, newEnv) -> do
          putStrLn ("Result is: " ++ show result)
          loop newEnv

        Left err ->
          putStrLn ("Error: " ++ showError err)

