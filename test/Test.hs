import Calculator
import System.Exit (exitFailure, exitSuccess)

testEval :: [(String, Either CalcError Double)] -> IO ()
testEval tests =
    runTests 0 0 tests
  where
    runTests :: Int -> Int -> [(String, Either CalcError Double)] -> IO ()
    runTests total passed [] = do
        putStrLn "==============================="
        putStrLn $ show passed ++ " of " ++ show total ++ " tests passed"
        putStrLn "=============================== \n"
        if passed == total
            then exitSuccess
            else exitFailure

    runTests total passed ((input, expected) : remaining) =
        let result = eval input
        in if result == expected
           then do
               putStrLn "- Passed"
               runTests (total + 1) (passed + 1) remaining
           else do
               putStrLn $ "x Test failed:"
               putStrLn $ " - Input: " ++ input
               putStrLn $ " - Expected: " ++ show expected
               putStrLn $ " - Output: " ++ show result
               runTests (total + 1) passed remaining

main :: IO ()
main = do
    testEval
        [ ("2 + 3", Right 5.0)
        , ("2 * 3", Right 6.0)
        , ("2 + 3 * 4", Right 14.0)
        , ("(2 + 3) * 4", Right 20.0)
        , ("10 / 2", Right 5.0)
        , ("10 / 0", Left DivisionByZero)
        , ("2^2^3", Right 256)
        ]
