module Calc.Lexer (tokenize) where

import Data.List (isPrefixOf, maximumBy)
import Data.Ord  (comparing)
import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)
import Calc.Token (Token(..))
import Data.Map (Map)
import Calc.Error
import qualified Data.Map as Map

symbolMap :: Map Char Token
symbolMap =
  Map.fromList
    [ ('(', TOpenParenthesis)
    , (')', TCloseParenthesis)
    , ('+', TPlus)
    , ('-', TMinus)
    , ('*', TMultiply)
    , ('/', TDivide)
    , ('^', TExponentiate)
    , (',', TComma)
    , ('=', TEquals)
    ]

tokenize :: String -> Either LexerError [Token]
tokenize [] = Right []
tokenize input@(c : rest)
  | isSpace c             = tokenize rest
  | isDigit c || c == '.' = do
      (token, rest') <- lexNumber input
      prepend token rest'
  | isAlpha c = do
      let (ident, rest') = span isAlphaNum input
      prepend (TIdentifier ident) rest'
  | otherwise =
    case Map.lookup c symbolMap of
      Just token -> prepend token rest
      Nothing    -> Left (InvalidOperator c)

prepend :: Token -> String -> Either LexerError [Token]
prepend token rest = (token :) <$> tokenize rest

lexNumber :: String -> Either LexerError (Token, String)
lexNumber input =
  case span isDigit input of
    (whole, '.' : afterDot) -> lexDecimal whole afterDot
    (whole, rest)           -> Right (TNumber (read whole), rest)

lexDecimal :: String -> String -> Either LexerError (Token, String)
lexDecimal whole afterDot
  | null whole || null fraction = Left (InvalidNumber number)
  | otherwise                   = Right (TNumber (read number), rest)
  where
    (fraction, rest) = span isDigit afterDot
    number           = whole ++ "." ++ fraction
