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

one = char '1'
one' = one >> stop

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"


pNL s =
  putStrLn ('\n' : s)  
newLinePrefix = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwoThree:"
  testParse oneTwo
  pNL "oneTwoThree':"
  testParse oneTwo'


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

-- terminate executation whenever finds 1st error
testParseFraction :: IO ()
testParseFraction = do
  print $ parseString parseFraction mempty badFraction  
  print $ parseString parseFraction mempty alsoBad
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork

-- execute all calls even if there is an error for first call
testVirtuous :: IO ()
testVirtuous = do
  print $ parseString virtuousFraction mempty badFraction
  print $ parseString virtuousFraction mempty alsoBad
  print $ parseString virtuousFraction mempty shouldWork
  print $ parseString virtuousFraction mempty shouldAlsoWork  


testInt n = n >> eof