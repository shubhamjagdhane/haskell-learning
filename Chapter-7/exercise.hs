module Excercise where

-- Grab bag

-- 3

  -- a

addOneIfOdd x = case odd x of
                True -> f x
                False -> x
                where f = \n -> n + 1

  -- b
addFive = \x -> \y -> (if x > y then y else x) + 5

  -- c

mflip f x y = f y x  

f x y = [y , x]


f' :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f' (a, _, c) (d, _, f) = ((a, d), (c, f))
