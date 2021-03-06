module Learned where

{-
sequenceA [Just 1, Just 2, Just 3]
Just [1,2,3]

general idea is that anytime you’re using sequenceA . fmap f, 
you can use traverse to achieve the same result in one step

In a literal sense, anytime you need to flip two type constructors
around, or map something and then flip them around, that’s probably Traversable

It’s usually better to use traverse whenever we see a sequence or sequenceA combined with a map or fmap

-}

data Either' a b = Left' a | Right' b deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Left' x) = Left' x
  fmap f (Right' y) = Right' (f y)

instance Applicative (Either' e) where
  pure = Right'
  Left' e <*> _ = Left' e
  Right' f <*> r = fmap f r

instance Foldable (Either' a) where
  foldMap _ (Left' _) = mempty
  foldMap f (Right' y) = f y
  foldr _ z (Left' _) = z
  foldr f z (Right' y) = f y z

instance Traversable (Either' a) where
  traverse _ (Left' x) = pure (Left' x)
  traverse f (Right' y) = Right' <$> f y


{-

instance Functor ((,) a) where
  fmap f (x,y) = (x, f y)

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)

instance Foldable ((,) a) where
  foldMap f (_, y) = f y
  foldr f z (_, y) = f y z

instance Traversable ((,) a) where
  traverse f (x, y) = (,) x <$> f

-}
