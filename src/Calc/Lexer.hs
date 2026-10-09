module Calc.Lexer (tokenize) where

import Calc.Error (Error (..), LexerError (..))
import Calc.Token (SourceSpan (..), Token (..), TokenType (..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Map (Map)
import qualified Data.Map as Map

symbolMap :: Map Char TokenType
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

-- | Spans are 0-based offsets into the input with an exclusive end.
--   The token list always ends with a TEOF token at the end of the input.
tokenize :: String -> Either Error [Token]
tokenize = go 0
  where
    go :: Int -> String -> Either Error [Token]
    go pos [] = Right [Token TEOF (SourceSpan pos pos)]
    go pos input@(c : rest)
      | isSpace c = go (pos + 1) rest
      | isDigit c || c == '.' = do
          (tokenType, rest') <- lexNumber pos input
          emit pos (length input - length rest') tokenType rest'
      | isAlpha c =
          let (ident, rest') = span isAlphaNum input
           in emit pos (length ident) (TIdentifier ident) rest'
      | otherwise =
          case Map.lookup c symbolMap of
            Just tokenType -> emit pos 1 tokenType rest
            Nothing -> Left (LexError (InvalidOperator c) (SourceSpan pos (pos + 1)))

    emit :: Int -> Int -> TokenType -> String -> Either Error [Token]
    emit pos len tokenType rest =
      (Token tokenType (SourceSpan pos (pos + len)) :) <$> go (pos + len) rest

lexNumber :: Int -> String -> Either Error (TokenType, String)
lexNumber pos input =
  case span isDigit input of
    (whole, '.' : afterDot) -> lexDecimal pos whole afterDot
    (whole, rest) -> Right (TNumber (read whole), rest)

lexDecimal :: Int -> String -> String -> Either Error (TokenType, String)
lexDecimal pos whole afterDot
  | null whole || null fraction =
      Left (LexError (InvalidNumber number) (SourceSpan pos (pos + length number)))
  | otherwise = Right (TNumber (read number), rest)
  where
    (fraction, rest) = span isDigit afterDot
    number = whole ++ "." ++ fraction
