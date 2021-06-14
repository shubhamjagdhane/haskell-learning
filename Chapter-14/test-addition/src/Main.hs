module Main where
import Addition

-- :set -package hspec

main :: IO ()
main = do
  num1 <- getLine
  num2 <- getLine 
  
  putStrLn (show $ add num1 num2)


