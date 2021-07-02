{-# LANGUAGE LambdaCase #-}

module MyParser where

import Text.Trifecta
import Control.Applicative
import Data.Ratio ((%))

stop :: Parser a
stop = unexpected "stop"

-- char :: Char -> Parser Char
-- char c =
--   Parser $ \case
--   (x : xs) -> ([(c, xs) | c == x])
--   _ -> []


{-
type Token = Char
newtype Parser a = P ([Token] -> [(a, [Token])])

-- Same thing, differently formatted:
type Parser' a = String -> [(a, String)]


> runStateT (put 8) 7
> runStateT get 8
> runStateT (put 1 >> get) 8
> (runStateT $ put 1 >> get) 0
> (runStateT $ put 2 >> get) 10021490234890
> (runStateT $ put 2 >> return 9001) 0
-}  


-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'

-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"


badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)  

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

testParseFraction :: IO ()
testParseFraction = do
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad
  print $ parseString parseFraction mempty badFraction  


testVirtuous :: IO ()
testVirtuous = do
  print $ parseString virtuousFraction mempty badFraction
  print $ parseString virtuousFraction mempty alsoBad
  print $ parseString virtuousFraction mempty shouldWork
  print $ parseString virtuousFraction mempty shouldAlsoWork  
