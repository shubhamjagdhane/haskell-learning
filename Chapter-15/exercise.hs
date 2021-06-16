import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only $ mappend x y


test (Only x) (Only y) = Only (mappend x y)
test Nada (Only y) = Only y
test (Only x) Nada = Only x
test Nada Nada = Nada
