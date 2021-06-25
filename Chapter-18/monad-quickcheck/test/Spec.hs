module Main where

import EitherTest
import CountMe
import Identity


import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b)=> Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left' b, Right' a]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where 
  (=-=) = eq 

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

main = do

  putStrLn "\t\t---------------- Testing CountMe ----------------"
  let countMe = undefined :: CountMe (Int, String, Int)
  quickBatch $ functor countMe
  quickBatch $ applicative countMe
  quickBatch $ monad countMe

  putStrLn "\n\t\t---------------- Testing PhhhbbtttEither ----------------"
  let phhhbbtttEither = undefined :: PhhhbbtttEither (Int, String, Int) (Int, String, Int)
  quickBatch $ functor phhhbbtttEither
  quickBatch $ applicative phhhbbtttEither
  quickBatch $ monad phhhbbtttEither
 
  putStrLn "\n\t\t---------------- Testing Identity ----------------"
  let identity = undefined :: Identity (Int, String, Int)
  quickBatch $ functor identity
  quickBatch $ applicative identity
  quickBatch $ monad identity