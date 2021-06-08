{-

map :: (a -> b) -> [a] -> [b]
map (+1) (1 : 2 : [])
(+1) 1 : (+1) 2 : []
[2,3]

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr (+) 0 (1 : 2 : [])
1 + ( 2 + 0 )
1 + 2
3

-}


fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
