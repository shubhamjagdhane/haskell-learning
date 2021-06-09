module Jammin where
import Data.List

data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Show, Ord)

data JamJars = Jam {fruit :: Fruit, howmany :: Int} deriving (Eq, Ord, Show)


row1 = Jam Peach 10
row2 = Jam Plum 20
row3 = Jam Apple 30
row4 = Jam Blackberry 25
row5 = Jam Apple 50

allJam = [row1, row2, row4, row3, row5]

-- total number of jars
totNumJar :: [JamJars] -> Int
totNumJar []     = 0 
totNumJar (x:xs) = howmany x + totNumJar xs 

-- jar having most number of jam
mostRow :: [JamJars] -> Maybe JamJars
mostRow []     = Nothing
mostRow (x:xs) = Just $ goMostRow x xs

goMostRow :: JamJars -> [JamJars] -> JamJars
goMostRow mostJar []     = mostJar
goMostRow mostJar (x:xs) = 
  case howmany mostJar < howmany x of
    True  -> goMostRow x xs
    False -> goMostRow mostJar xs

sortJarByCount :: [JamJars] -> [JamJars]
sortJarByCount xs = sortBy (\x y -> compare (howmany x) (howmany y)) xs

-- working only for adjacent records
groupByFruit :: [JamJars] -> [[JamJars]]
groupByFruit xs = groupBy isSame xs

isSame :: JamJars -> JamJars -> Bool
isSame x y = (fruit x) == (fruit y)
