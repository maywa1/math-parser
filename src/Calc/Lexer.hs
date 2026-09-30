module Calc.Lexer (tokenize) where

import Data.Char (isDigit, isSpace)
import Calc.Token (Token(..))
import Calc.Error (CalcError(..))

tokenize :: String -> Either CalcError [Token]
tokenize [] = Right []
tokenize ('(' : rest) = prepend TOpenParenthesis rest
tokenize (')' : rest) = prepend TCloseParenthesis rest
tokenize ('+' : rest) = prepend TPlus rest
tokenize ('-' : rest) = prepend TMinus rest
tokenize ('*' : rest) = prepend TMultiply rest
tokenize ('/' : rest) = prepend TDivide rest
tokenize ('^' : rest) = prepend TExponentiate rest
tokenize input@(c : rest)
  | isSpace c             = tokenize rest
  | isDigit c || c == '.' = do
      (token, rest') <- lexNumber input
      prepend token rest'
  | otherwise             = Left (InvalidOperator c)

prepend :: Token -> String -> Either CalcError [Token]
prepend token rest = (token :) <$> tokenize rest

lexNumber :: String -> Either CalcError (Token, String)
lexNumber input =
  case span isDigit input of
    (whole, '.' : afterDot) -> lexDecimal whole afterDot
    (whole, rest)           -> Right (TNumber (read whole), rest)

lexDecimal :: String -> String -> Either CalcError (Token, String)
lexDecimal whole afterDot
  | null whole || null fraction = Left (InvalidNumber number)
  | otherwise                   = Right (TNumber (read number), rest)
  where
    (fraction, rest) = span isDigit afterDot
    number           = whole ++ "." ++ fraction
