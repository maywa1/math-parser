import Calc.Evaluator (eval)
import Calc.Error (CalcError(..))
import Calc.Token (Token(..))
import System.Exit (exitFailure, exitSuccess)

testEval :: [(String, Either CalcError Double)] -> IO ()
testEval tests =
    runTests 0 0 tests
  where
    runTests :: Int -> Int -> [(String, Either CalcError Double)] -> IO ()
    runTests total passed [] = do
        putStrLn "==============================="
        putStrLn $ show passed ++ " of " ++ show total ++ " tests passed"
        putStrLn "===============================\n"
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

        -- unary minus
        , ("-5", Right (-5.0))
        , ("-5 + 3", Right (-2.0))
        , ("3 + -5", Right (-2.0))
        , ("3 - -5", Right 8.0)
        , ("3 * -5", Right (-15.0))
        , ("-5 * -5", Right 25.0)
        , ("3 / -5", Right (-0.6))
        , ("-2^2", Right (-4.0))       -- unary minus has lower precedence than ^
        , ("(-2)^2", Right 4.0)
        , ("-3 - -3", Right 0.0)
        , ("--5", Right 5.0)
                -- left associativity
        , ("10 - 3 - 2", Right 5.0)          -- would be 9.0 if right-associative
        , ("10 - 3 - 2 - 1", Right 4.0)
        , ("100 / 10 / 2", Right 5.0)        -- would be 20.0 if right-associative
        , ("100 / 5 / 2 / 2", Right 5.0)
        , ("20 - 5 * 2", Right 10.0)
        , ("20 - 10 / 2", Right 15.0)
        , ("2 - 3 + 4", Right 3.0)           -- mixed +/- left-to-right
        , ("2 - 3 + 4 - 1", Right 2.0)

        -- right associativity
        , ("2^3^2", Right 512.0)             -- 2^(3^2) = 2^9, not (2^3)^2 = 64

        -- decimals
        , ("2.5 + 2.5", Right 5.0)
        , ("10.5 / 2", Right 5.25)
        , ("1.5 * 1.5", Right 2.25)

        -- parentheses / nesting
        , ("((2 + 3))", Right 5.0)
        , ("(2 + 3) - (1 + 1)", Right 3.0)
        , ("2 * (3 + (4 - 1))", Right 12.0)
        , ("(10 - 4) / (1 + 2)", Right 2.0)

        -- whitespace handling
        , ("  2   +   3  ", Right 5.0)
        , ("2+3*4", Right 14.0)

        -- errors
        , ("2 + ", Left UnexpectedEndOfExpression)
        , ("(2 + 3", Left MissingParenthesis)
        , ("2 + 3)", Left (SyntaxError TCloseParenthesis))
        , ("2 @ 3", Left (InvalidOperator '@'))
        , ("2..5 + 1", Left (InvalidNumber "2."))
        -- trig functions (exact values)
        , ("sin(0)", Right 0.0)
        , ("cos(0)", Right 1.0)
        , ("tan(0)", Right 0.0)

        -- trig functions (compare against Prelude so float rounding can't bite)
        , ("sin(1)", Right (sin 1.0))
        , ("cos(1)", Right (cos 1.0))
        , ("tan(1)", Right (tan 1.0))
        , ("sin(-1)", Right (sin (-1.0)))
        , ("cos(2.5)", Right (cos 2.5))

        -- functions inside expressions
        , ("sin(0) + 3", Right 3.0)
        , ("2 * cos(0)", Right 2.0)
        , ("2 + 3 * cos(0)", Right 5.0)
        , ("cos(0) ^ 3", Right 1.0)
        , ("-cos(0)", Right (-1.0))
        , ("-sin(1)", Right (negate (sin 1.0)))
        , ("cos(0) - -cos(0)", Right 2.0)

        -- arguments that are full expressions
        , ("sin(1 + 1)", Right (sin 2.0))
        , ("cos(2 * 0)", Right 1.0)
        , ("sin((1 + 1) * 2)", Right (sin 4.0))
        , ("cos(sin(0))", Right 1.0)           -- nested functions
        , ("sin(cos(0))", Right (sin 1.0))
        , ("sin(0 - 1)", Right (sin (-1.0)))

        -- whitespace around functions
        , ("sin( 0 )", Right 0.0)
        , ("  cos(0)  ", Right 1.0)

        -- function errors
        , ("sin(", Left UnexpectedEndOfExpression)
        , ("sin(0", Left MissingParenthesis)
        , ("sin()", Left UnexpectedEndOfExpression)
        , ("sin", Left UnexpectedEndOfExpression)
        , ("tan(1 / 0)", Left DivisionByZero)

        -- implicit multiplication (not supported yet, so these should fail)
        , ("2(3)", Left (SyntaxError TOpenParenthesis))
        , ("(2)(3)", Left (SyntaxError TOpenParenthesis))
        , ("(2 + 3)(4 + 5)", Left (SyntaxError TOpenParenthesis))
        , ("2 3", Left (SyntaxError (TNumber 3.0)))
        , ("2 cos(0)", Left (SyntaxError TCosine))
        , ("2cos(0)", Left (SyntaxError TCosine))
        , ("(2)cos(0)", Left (SyntaxError TCosine))
        , ("cos(0)2", Left (SyntaxError TCosine))
        , ("cos(0)(2)", Left (SyntaxError TOpenParenthesis))
        , ("sin(0) cos(0)", Left (SyntaxError TCosine))
        ]
