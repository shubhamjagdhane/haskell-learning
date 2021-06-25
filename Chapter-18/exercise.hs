module Exercise where

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)
  
instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second
  First f <*>  _ = First f
  Second f <*> g = fmap f g
  
instance Monad (Sum a) where
  return x       = Second x
  First f >>= _  = First f
  Second f >>= g =  g f
