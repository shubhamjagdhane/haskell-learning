module Tree where

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty        = Empty
  fmap f (Leaf a)     = Leaf $ f a
  fmap f (Node l k r) = Node (fmap f l) (f k) (fmap f r)

instance Applicative Tree where
  pure x                      = Leaf x
  Empty <*> _                 = Empty
  Leaf f <*> g                = fmap f g
  Node left value right <*> g = fmap value g -- this is not a recursive, trying to find out solution for this
    
    
  -- Node (left <*> fmap value g) Empty (right <*> fmap value g) 
  
  -- Node (left <*> g) () (right <*> g)

instance Foldable Tree where
  foldMap f Empty        = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node l k r) = foldMap f l <> f k <> foldMap f r

  foldr f z Empty        = z
  foldr f z (Leaf x)     = f x z
  foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l

instance Traversable Tree where
  traverse f Empty        = pure Empty
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r