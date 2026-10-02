module Calc.Lexer (tokenize) where

import Data.List (isPrefixOf, maximumBy)
import Data.Ord  (comparing)
import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)
import Calc.Token (Token(..))
import Calc.Error (CalcError(..))
import Calc.Map (Map, lookUp)

identMap :: Map String Token
identMap =
  [ ("pi" , TNumber pi)
  , ("cos", TFunction "cos")
  , ("sin", TFunction "sin")
  , ("tan", TFunction "tan")
  ]

symbolMap :: Map Char Token
symbolMap =
  [ ('('  , TOpenParenthesis)
  , (')'  , TCloseParenthesis)
  , ('+'  , TPlus)
  , ('-'  , TMinus)
  , ('*'  , TMultiply)
  , ('/'  , TDivide)
  , ('^'  , TExponentiate)
  ]

tokenize :: String -> Either CalcError [Token]
tokenize [] = Right []
tokenize input@(c : rest)
  | isSpace c             = tokenize rest
  | isDigit c || c == '.' = do
      (token, rest') <- lexNumber input
      prepend token rest'
  | isAlpha c = do
      let (ident, rest') = span isAlphaNum input
      case lookup ident identMap of
        Just token -> prepend token rest'
        Nothing    -> Left (UndefinedIdentifier ident)
  | otherwise =
    case lookup c symbolMap of
      Just token -> prepend token rest
      Nothing    -> Left (InvalidOperator c)

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
