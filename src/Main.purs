module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.Foldable (fold)
import Data.Maybe
import Data.String (splitAt)
import Data.Char
import Data.String.CodeUnits
import Data.Tuple (Tuple(..))

data JsonValue = JsonNull
               | JsonBool Boolean

instance showJsonValue :: Show JsonValue where
  show JsonNull = "JsonNull"
  show (JsonBool a) = "JsonBool " <> (show a)

type Parser a = String -> Maybe (Tuple String a)

charP :: Char -> Parser Char
charP c = \str ->
  case (splitAt 1 str) of
    {before:y, after:ys}
      |  y == (singleton c) -> Just (Tuple ys c)
    _ -> Nothing

runParser :: forall a. Parser a -> String -> Maybe (Tuple String a)
runParser p str = p str

main :: Effect Unit
main = logShow $
  runParser (charP 'n') "nice"
