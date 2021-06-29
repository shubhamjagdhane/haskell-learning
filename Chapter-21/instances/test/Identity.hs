module Identity where


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity e <*> f = fmap e f
 

instance Foldable Identity where
  foldMap f (Identity y) = f y
  foldr f z (Identity y) = f y z

instance Traversable Identity where
  traverse f (Identity y) = Identity <$> f y