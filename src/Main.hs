module Main where
import Calculator

main :: IO ()
main = do
    putStrLn "Type your expression:"
    input <- getLine

    case eval input of
        Right result ->
            putStrLn ("Result is: " ++ show result)

        Left err ->
            putStrLn ("Error: " ++ showError err)
