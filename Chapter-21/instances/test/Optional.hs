module Optional where

data Optional a = Nada | Yep a deriving (Show, Eq)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep y) = Yep (f y)

instance Applicative Optional where
  pure        = Yep
  Nada <*> _  = Nada
  Yep f <*> r = fmap f r

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep y) = f y
  foldr _ z Nada    = z
  foldr f z (Yep y) = f y z

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep y) = Yep <$> f y