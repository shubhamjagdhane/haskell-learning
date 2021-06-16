module Exercise where
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

firstGenEqual :: (Arbitrary a) => Gen (First' a)
firstGenEqual = do
  a <- arbitrary
  oneof [return $ First' (Only a),  return $ First' Nada]

instance Arbitrary a => Arbitrary (First' a) where 
  arbitrary = firstGenEqual

instance Semigroup a => Semigroup (First' a) where
  (<>) (First' (Only a)) _             = First' {getFirst' = Only a}
  (<>) (First' Nada) (First' (Only a)) = First' {getFirst' = Only a}
  (<>) (First' Nada) (First' Nada)     = First' {getFirst' = Nada}

instance Monoid a => Monoid (First' a) where
  mempty = First' {getFirst' = Nada}
  mappend (First' (Only x)) (First' (Only y)) = First' {getFirst' = Only x}
  mappend (First' Nada) (First' (Only x))     = First' {getFirst' = Only x}
  mappend (First' (Only x)) (First' Nada)     = First' {getFirst' = Only x}
  mappend (First' Nada) (First' Nada)         = First' {getFirst' = Nada}


firstMappend :: First' a -> First' a -> First' a
firstMappend (First' (Only x)) (First' (Only y))  = First' {getFirst' = Only x}