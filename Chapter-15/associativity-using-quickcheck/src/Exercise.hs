module Exercise where
import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada

  mappend (First' (Only x)) (First' (Only y)) = First' (Only x)

  mappend (First' (Only x)) Nada = First' (Only x)

  mappend (First' Nada) (First' (Only x))_ = First' (Only x) 

firstMappend :: First' a -> First' a -> First' a
firstMappend (First' x) (First' y)  = First' $ mappend (First' x) (First' y)