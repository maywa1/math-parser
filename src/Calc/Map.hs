module Calc.Map (Map, lookUp) where

type Map k a = [(k, a)]

lookUp :: Eq k => k -> Map k a -> Maybe a
lookUp key map =
  case map of
    [] -> Nothing
    (currentKey, e) : rest ->
      if key == currentKey
      then Just e
      else lookUp key rest

