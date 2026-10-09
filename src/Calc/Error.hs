module Calc.Error where

import Calc.Token (SourceSpan (..))
import Data.List (intercalate)

data Error
  = EvaluationError EvaluationError (Maybe SourceSpan)
  | LexError LexerError SourceSpan
  | ParseError ParserError SourceSpan
  deriving (Show, Eq)

data EvaluationError
  = InvalidArgumentCount Int Int -- expected, actual
  | ExpectedNumber
  | DivisionByZero
  | UndefinedVariable String
  | UndefinedFunction String
  deriving (Show, Eq)

data LexerError
  = InvalidOperator Char
  | InvalidNumber String
  deriving (Show, Eq)

data ParserError
  = DuplicateParameter String
  | SyntaxError
  | UnexpectedEndOfExpression
  | MissingParenthesis
  deriving (Show, Eq)

-- | Render an error together with the input it came from, pointing at the
--   offending span with carets:
--
-- > Syntax error
-- >   1 + * 2
-- >       ^
showError :: Error -> String -> String
showError err input =
  case errorSpan err of
    Nothing -> message
    Just sp ->
      intercalate "\n" [message, "  " ++ input, "  " ++ pointer sp]
  where
    message = errorMessage err

-- | Spans are 0-based offsets into the input, with an exclusive end.
--   Zero-width spans (e.g. the EOF token) still get a single caret.
pointer :: SourceSpan -> String
pointer (SourceSpan start end) =
  replicate start ' ' ++ replicate (max 1 (end - start)) '^'

errorSpan :: Error -> Maybe SourceSpan
errorSpan (EvaluationError _ sp) = sp
errorSpan (LexError _ sp) = Just sp
errorSpan (ParseError _ sp) = Just sp

errorMessage :: Error -> String
errorMessage (ParseError err _) =
  case err of
    DuplicateParameter param -> "Duplicate parameter: " ++ param
    SyntaxError -> "Syntax error"
    UnexpectedEndOfExpression -> "Unexpected end of expression!"
    MissingParenthesis -> "You forgot to close parenthesis here"
errorMessage (LexError err _) =
  case err of
    InvalidOperator c -> [c] ++ " is not a valid operator!"
    InvalidNumber n -> n ++ " is not a valid number!"
errorMessage (EvaluationError err _) =
  case err of
    UndefinedVariable str -> "Undefined variable: " ++ str
    UndefinedFunction str -> "Undefined function: " ++ str
    InvalidArgumentCount expected actual ->
      "Invalid argument count. Expected: " ++ show expected ++ ", got: " ++ show actual
    ExpectedNumber -> "Expected a number"
    DivisionByZero -> "Cannot divide by zero"
