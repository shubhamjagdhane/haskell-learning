module Main where

import Exercise

import Associative
    ( monoidAssoc, monoidLeftIdentity, monoidRightIdentity )
import Test.QuickCheck
import Data.Monoid


{-
data Bull = Fools| Twoo deriving (Eq, Show)

instance Semigroup Bull where
  (<>) Fools Twoo = Fools

instance Arbitrary Bull where 
  arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]

instance Monoid Bull where
  mempty      = Fools
  mappend _ _ = Fools


type BullMappend = Bull -> Bull -> Bull -> Bool
-}

type FirstMappend = First' String -> First' String -> First' String -> Bool

main :: IO ()
main = do  
  -- this is suppose to fail
  -- quickCheck (monoidAssoc :: BullMappend)
  -- quickCheck (monoidLeftIdentity :: Bull -> Bool)
  -- quickCheck (monoidRightIdentity :: Bull -> Bool)

  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
