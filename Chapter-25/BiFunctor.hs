import Data.Bifunctor

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
    bimap f _ (Left' a) = Left' (f a)
    bimap _ g (Right' b) = Right' (g b)

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

newtype Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a) 

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)