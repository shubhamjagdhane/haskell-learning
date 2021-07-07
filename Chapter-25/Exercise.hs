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

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  -- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
  foldMap f (Compose t) = (foldMap . foldMap) f t

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose t) = Compose <$> (traverse . traverse) f t