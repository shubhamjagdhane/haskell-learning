module Learned where

newtype Identity a = Identity a deriving (Show, Eq)

instance Foldable  Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x


{-

foldr (*) 1 (Identity 5)
foldl (*) 5 (Identity 5)
foldMap (*5) (Identity 100) :: Product Integer

I/P = foldr (+) 1 Nothing
O/P = 1

I/P = foldMap (+1) Nothing :: Sum Integer
O/P = Sum {getSum = 0}

-}  

data Optional a = Nada | Yep a deriving (Show, Eq)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

-- toList :: t a -> [a]
-- concatMap toList [Just 1, Just 2, Just 3]

{-
null, returns True for Left, Nothing and []

null (Left 3)
null Nothing


fmap (elem 3) [Right 1, Right 2, Right 3]
[False,False,True]


fmap maximum [Just 2, Just 10, Just 4]
[2,10,4]
fmap maximum (Just [3, 7, 10, 2])
Just 10

-}

