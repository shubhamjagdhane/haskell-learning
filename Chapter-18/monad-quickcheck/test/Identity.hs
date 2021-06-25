module Identity where
  
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> g = fmap f g

instance Monad Identity where
  return = pure
  Identity f >>= g = g f