module Exercise where
import Data.Monoid

-- sum
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

-- product
product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

-- elem
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = any (==x) xs -- any . (==)

newtype Constant a b = Constant a deriving (Show, Eq)

instance Foldable (Constant a) where
  foldr _ z (Constant x) = z

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p = foldMap (\a -> if p a then pure a else mempty)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) 
  | f x = x : filter' f xs 
  | otherwise  = filter' f xs