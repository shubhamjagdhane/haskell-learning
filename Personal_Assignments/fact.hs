module SafeFact where

data MaybeInt = Success Integer | Stopped String
instance Show MaybeInt where
  show (Success x) = show x
  show (Stopped y) = show y

fact :: Integer -> MaybeInt
fact 0 = Success 1
fact x = case x > 0 of
         True -> go x 1
         False -> Stopped "Please pass positive number!"

go :: Integer -> Integer -> MaybeInt
go 1 n = Success n
go x n = go (x-1) (n*x)
