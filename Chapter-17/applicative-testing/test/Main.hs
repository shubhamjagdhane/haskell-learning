module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]

instance Semigroup Bull where
  (<>) Fools Fools = Fools
  (<>) Fools Twoo  =  Fools
  (<>) Twoo Fools  =  Fools
  (<>) Twoo Twoo   =  Twoo

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools
instance EqProp Bull where (=-=) = eq

trigger = undefined :: [(String, String, Int)]


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
 


main :: IO ()
main = do
  quickBatch (applicative trigger)
