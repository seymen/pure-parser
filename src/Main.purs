module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.Maybe (Maybe(..))
import Data.String (splitAt)
import Data.String.CodeUnits (singleton)
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

instance applicativeParser :: Applicative Parser where
  pure x = Parser $ \str -> Just (Tuple str x)

instance applyParser :: (Functor Parser) => Apply Parser where
  apply p1 p2 = Parser $ \str -> do
     Tuple input' f <- runParser p1 str
     Tuple input'' a <- runParser p2 input'
     Just (Tuple input'' (f a))

charP :: Char -> Parser Char
charP c = Parser $ \str ->
  case (splitAt 1 str) of
    {before:y, after:ys}
      |  y == (singleton c) -> Just (Tuple ys c)
    _ -> Nothing

main :: Effect Unit
main = logShow $
  runParser (charP 'n') "nice"
