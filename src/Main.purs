module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.Array (span)
import Data.Maybe (Maybe(..))
import Data.String (splitAt)
import Data.String.CodeUnits (singleton, toCharArray, fromCharArray)
import Data.Tuple (Tuple(..))
import Data.Traversable (sequenceDefault)
import Control.Alt (class Alt, (<|>))
import Unsafe.Coerce (unsafeCoerce)
import Data.Char.Unicode (isDigit)
import Data.Int (fromString)

data JsonValue = JsonNull
               | JsonBool Boolean
               | JsonNumber Int
               | JsonString String

instance showJsonValue :: Show JsonValue where
  show JsonNull = "JsonNull"
  show (JsonBool a) = "JsonBool " <> (show a)
  show (JsonNumber a) = "JsonNumber " <> (show a)
  show (JsonString a) = "JsonString " <> "\"" <> a <> "\""

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

instance altParser :: (Functor Parser) => Alt Parser where
  alt p1 p2 = Parser $ \str -> (runParser p1 str) <|> (runParser p2 str)

charP :: Char -> Parser Char
charP c = Parser $ \str ->
  case (splitAt 1 str) of
    {before:y, after:ys}
      |  y == (singleton c) -> Just (Tuple ys c)
    _ -> Nothing

stringP :: String -> Parser String
stringP = map fromCharArray <<< sequenceDefault <<< map charP <<< toCharArray

ifP :: (Char -> Boolean) -> Parser String
ifP pred =
  Parser \str ->
    let {init, rest} = span pred $ toCharArray str
    in Just (Tuple (fromCharArray rest) (fromCharArray init))

jsonNullP :: Parser JsonValue
jsonNullP = map (\_ -> JsonNull) $ stringP "null"

jsonBoolP :: Parser JsonValue
jsonBoolP = map f ((stringP "true") <|> (stringP "false"))
  where f "true"  = JsonBool true
        f "false" = JsonBool false
        f _       = unsafeCoerce JsonBool

jsonNumberP :: Parser JsonValue
jsonNumberP = Parser $ \str -> do
  Tuple input' digitsAsStr <- runParser (ifP isDigit) str
  int' <- fromString digitsAsStr
  Just (Tuple input' (JsonNumber int'))

jsonStringP :: Parser JsonValue
jsonStringP = JsonString <$> (charP '"' *> stringLiteral <* charP '"')
  where stringLiteral = ifP (notEq '"')

jsonP :: Parser JsonValue
jsonP = jsonNullP <|> jsonBoolP <|> jsonNumberP <|> jsonStringP

main :: Effect Unit
main = logShow $
  runParser jsonP "123\"hello\""
  {-- runParser numberP "2343" --}
  {-- runParser (ifP isDigit) "23432" --}
  {-- runParser boolP "false" --}
  {-- runParser nullP "nullable" --}
  {-- runParser (stringP "null") "nullable" --}
  {-- runParser (charP 'n') "nice" --}
