module Fact where

-- not working
--fact :: (Num a, String a) => a -> a
--fact 0 = 1
--fact n = if n < 0 then "please enter positive number" else n * fact n-1

data MaybeInt = MayInt Integer | MayString String
fact' :: Integer -> MaybeInt
fact' 0 = MayInt 1
fact' n = case MayInt 

-- not working
--fact'' :: a -> a
--fact'' 0 = 1
--fact'' n = if n < 0 then "please enter positive number" else n * fact'' n-1


