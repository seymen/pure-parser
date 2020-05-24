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

newtype Parser a = Parser (String -> Maybe (Tuple String a))

runParser :: forall a. Parser a -> String -> Maybe (Tuple String a)
runParser (Parser p) str = p str

instance functorParser :: Functor Parser where
  map f p = Parser $ \str -> do
     Tuple input' x <- runParser p str
     Just (Tuple input' (f x))

charP :: Char -> Parser Char
charP c = Parser $ \str ->
  case (splitAt 1 str) of
    {before:y, after:ys}
      |  y == (singleton c) -> Just (Tuple ys c)
    _ -> Nothing

main :: Effect Unit
main = logShow $
  runParser (charP 'n') "nice"
