module Listy where 

import Data.Monoid

newtype Listy a = Listy [a] deriving (Eq, Show)
instance Semigroup a => Semigroup (Listy a) where
  (<>) (Listy l) (Listy l') = Listy $ mappend l l'

instance Monoid a => Monoid (Listy a) where
  mempty = Listy []
  mappend (Listy l) (Listy l') =  Listy $ mappend l l'
