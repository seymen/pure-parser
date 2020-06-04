module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.Array (span, (:), many)
import Data.Maybe (Maybe(..))
import Data.String (splitAt)
import Data.String.CodeUnits (singleton, toCharArray, fromCharArray)
import Data.Tuple (Tuple(..))
import Data.Traversable (sequenceDefault)
import Control.Plus (class Alt, class Plus, (<|>))
import Control.Lazy (class Lazy)
import Control.Alternative (class Alternative)
import Unsafe.Coerce (unsafeCoerce)
import Data.Char.Unicode (isDigit, isSpace)
import Data.Int (fromString)

data JsonValue = JsonNull
               | JsonBool Boolean
               | JsonNumber Int
               | JsonString String
               | JsonObject (Array (Tuple String JsonValue))

instance showJsonValue :: Show JsonValue where
  show JsonNull = "JsonNull"
  show (JsonBool a) = "JsonBool " <> (show a)
  show (JsonNumber a) = "JsonNumber " <> (show a)
  show (JsonString a) = "JsonString " <> "\"" <> a <> "\""
  show (JsonObject a) = "JsonObject " <> (show a)

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

instance plusParser :: (Alt Parser) => Plus Parser where
  empty = Parser $ \_ -> Nothing

instance alternativeParser :: (Applicative Parser, Plus Parser) => Alternative Parser

instance lazyParser :: Lazy (Parser (Array a)) where
  defer g = Parser (\s -> runParser (g unit) s)

charP :: Char -> Parser Char
charP c = Parser $ \str ->
  case (splitAt 1 str) of
    {before:y, after:ys}
      |  y == (singleton c) -> Just (Tuple ys c)
    _ -> Nothing

stringP :: String -> Parser String
stringP = map fromCharArray <<< sequenceDefault <<< map charP <<< toCharArray

spaceP :: Parser String
spaceP = ifP isSpace

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

stringLiteral :: Parser String
stringLiteral = charP '"' *> ifP (notEq '"') <* charP '"'

jsonStringP :: Parser JsonValue
jsonStringP = JsonString <$> stringLiteral

sepBy :: forall a b. Parser a -> Parser b -> Parser (Array b)
sepBy sep element = (:) <$> element <*> many (sep *> element)

jsonObjectP :: Parser JsonValue
jsonObjectP = JsonObject <$>
  (charP '{' *> spaceP *> sepBy (spaceP *> charP ',' <* spaceP) pair <* spaceP <* charP '}')
  where pair = (\key _ value -> Tuple key value) <$> stringLiteral <*> (spaceP *> charP ':' <* spaceP) <*> jsonValueP

jsonValueP :: Parser JsonValue
jsonValueP = jsonNullP <|> jsonBoolP <|> jsonNumberP <|> jsonStringP

main :: Effect Unit
main = logShow $
  {-- runParser $ sepBy (charP ',') (stringP "1,2,3,4") --}
  {-- runParser jsonObjectP "{}" --}
  runParser jsonObjectP "{\"a\": \"b\", \"b\":123}"
  {-- runParser numberP "2343" --}
  {-- runParser (ifP isDigit) "23432" --}
  {-- runParser boolP "false" --}
  {-- runParser nullP "nullable" --}
  {-- runParser (stringP "null") "nullable" --}
  {-- runParser (charP 'n') "nice" --}
