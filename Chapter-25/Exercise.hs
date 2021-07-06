{-# LANGUAGE InstanceSigs #-}
module Exercise where


newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga  

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  -- pure :: a -> Compose f g a
  pure = Compose . pure . pure
  (Compose f) <*> (Compose g) = Compose ((<*>) <$>f <*> g)

  -- (+) <$> (Compose (Just [1,2,3])) <*> (Compose (Just [4,5,6]))
{-
incomplete exercise


instance (Monad f, Monad g) => Monad (Compose f g) where
  return = pure  


IdentityT $ join (fmap (runIdentityT . f) ma)

IdentityT $ ma >>= runIdentityT . f

-}
