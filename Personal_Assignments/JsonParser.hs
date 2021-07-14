module JsonParser where

import Control.Applicative
import Data.Char


data JsonValue = 
    JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- Note: no supports for floats
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- no proper error reporting
newtype Parser a =
  Parser {
    runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser g) = Parser go
    where
      go input = do
        (s', a) <- g input
        return (s', f a)

instance Applicative Parser where
  pure x = Parser go
    where
      go input = do
        return (input, x)

  (Parser f) <*> (Parser g) = Parser go
    where
      go input = do
        (s', fab) <- f input
        (s'', a)  <- g s'
        return (s'', fab a)

instance Alternative Parser where
  empty = Parser $ \_ ->  Nothing
  (Parser a) <|> (Parser b) = Parser go
    where
      go input = a input <|> b input

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true"  = JsonBool True
    f "false" = JsonBool False
    f _       = undefined

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
  where f ds = JsonNumber $ read ds

stringLiteral :: Parser String
stringLiteral = (charP '"' *> (spanP (/= '"')) <* charP '"')

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> 
  (charP '[' 
    *> ws *> elements <* ws <* 
  charP ']')
  where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue


jsonObject :: Parser JsonValue
jsonObject = 
  JsonObject <$>  
  (charP '{' 
  *> ws *> 
  sepBy (ws *> charP ',' <* ws) pair 
  <* ws <* 
  charP '}')
  where
    pair =
      (\key _ value -> (key, value))
      <$> stringLiteral 
      <*> (ws *> charP ':' <* ws)
      <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser go
  where
    go input = do
      (input', xs) <- p input
      if null xs
      then Nothing
      else Just (input', xs)

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser go
  where
    go = Just <$> flipT . span f

flipT :: (a, b) -> (b, a)
flipT (a, b) = (b, a)

charP :: Char -> Parser Char
charP x = Parser f
  where 
    f [] = Nothing
    f (y:ys) 
      | y == x    = Just (ys, x)
      | otherwise = Nothing


stringP :: String -> Parser String
stringP = sequenceA . map charP
