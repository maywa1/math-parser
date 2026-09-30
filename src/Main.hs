module Main where
import Calc.Evaluator (eval)
import Calc.Error (showError)

main :: IO ()
main = do
    putStrLn "Type your expression:"
    input <- getLine

    case eval input of
        Right result ->
            putStrLn ("Result is: " ++ show result)

        Left err ->
            putStrLn ("Error: " ++ showError err)
