module FunctionWithWhereAndLet where

-- function with where
printInc1 n = print plusTwo
  where plusTwo = n + 1

printInc1' = \n -> n + 2


-- function with let
printInc2 n = let plusTwo = n + 2
              in print plusTwo

-- using lambda
printInc2' n = 
  (\plusTwo -> print plusTwo) (n+2)

-- identity lambda
identity = (\x ->x)
