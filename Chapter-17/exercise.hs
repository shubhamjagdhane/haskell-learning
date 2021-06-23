module Exercise where
import Data.List (elemIndex)

-- Lookups

-- 1
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1..3] [4..6])

-- 2
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Maybe Int -> Maybe Int -> Maybe Int
max' = max 

maxed :: Maybe Int
maxed = max' x y'


-- 4

xs = [1, 2, 3]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed =  sum <$> ((,) <$> x'' <*> y'') 

-- Identity instance


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure               = Identity
  (Identity f) <*> x = fmap f x 

-- Constant instance

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant x) = Constant { getConstant = x }

 
