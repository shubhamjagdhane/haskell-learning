module EitherTest where

data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Show, Eq)

instance Functor (PhhhbbtttEither a) where
  fmap _ (Right' x) = Right' x
  fmap f (Left' x) = Left' (f x) 

instance Applicative (PhhhbbtttEither a) where
  pure = Left'
  Right' f <*> _  = Right' f
  Left' f <*> g = fmap f g

instance Monad (PhhhbbtttEither a) where
  return x       = Left' x
  Right' f >>= _  = Right' f
  Left' f >>= g =  g f  

