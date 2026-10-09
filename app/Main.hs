module Main where

import Calc (run)
import Calc.Error (showError)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)

main :: IO ()
main = loop Map.empty
  where
    loop env = do
      putStr "> "
      hFlush stdout
      input <- getLine

      case run env input of
        Right (result, newEnv) -> do
          putStrLn ("Result is: " ++ show result)
          loop newEnv

        Left err ->
          putStrLn ("Error: " ++ showError err input)
