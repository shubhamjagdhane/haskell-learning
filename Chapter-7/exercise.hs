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


-- case expression

functionC x y = case x > y of
                  True -> x
                  False -> y


ifEvenAdd2 n = case even n of
                True -> n + 2
                False -> n

nums x = case compare x 0 of
          LT -> (-1)
          GT -> 1
          EQ -> 0
