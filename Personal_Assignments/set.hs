-- foldr
fold :: (a -> b -> b) -> b -> [a] -> b
fold f empty [] = empty
fold f empty (x:xs) = x `f` (fold f empty xs)

data Set a = Empty | Sing a | Union (Set a) (Set a) deriving Show

foldSet :: b -> (a -> b) -> (b -> b -> b) -> Set a -> b 
foldSet e s u Empty       = e
foldSet e s u (Sing x)    = s x
foldSet e s u (Union x y) = (foldSet e s u x) `u` (foldSet e s u y)

isIn :: (Eq a) => a -> Set a -> Bool
isIn x = foldSet False (==x) (||)

subst :: (Eq a) => Set a -> Set a -> Bool
subst s1 s2 = foldSet True (`isIn` s2) (&&) s1

testSet1 = Union (Sing 1) (Union (Sing 2) Empty)
testSet2 = Union (Sing 1) (Union (Sing 2) (Sing 3))

instance Eq a => Eq (Set a) where
  s == r = (s `subst` r) && (r `subst` s)
