module Main where

main :: IO ()
main = do
  c  <- getLine
  c' <- getLine
  if c == c'
  then putStrLn "True"
  else return ()