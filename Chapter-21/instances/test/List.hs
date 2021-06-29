module List where

data List a = Nil | Cons a (List a) deriving (Show, Eq)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) =  Cons (f x) (fmap f xs)

instance Applicative List where
  pure x                   = Cons x Nil
  Nil <*> _                = Nil
  Cons f Nil <*> g         = fmap f g
  Cons f (Cons x xs) <*> g = append (fmap f g) (Cons x xs <*> g)

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons y ys) = mappend (f y) (foldMap f ys)

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs