module Main where

import Constant
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

{-
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
-}

data List a = Nil | Cons a (List a) deriving (Show, Eq)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  Cons f Nil <*> g = fmap f g
  Cons f (Cons x xs) <*> g = append (fmap f g) (Cons x xs <*> g)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
  a <- arbitrary
  elements [Nil, Cons a Nil]


instance Semigroup a => Semigroup (List a) where
  (<>) Nil xs = xs
  (<>) xs Nil  =  xs
  (<>) Nil Nil  =  Nil
  (<>) (Cons x xs) ys  =  Cons x $ (<>) xs ys

instance Monoid a => Monoid (List a) where
  mempty = Nil
  mappend Nil xs = xs
  mappend xs Nil  =  xs
  mappend Nil Nil  =  Nil
  mappend (Cons x xs) ys  =  Cons x $ xs `mappend` ys

instance (Eq a) => EqProp (List a) where 
  (=-=) = eq

trigger = undefined :: [(String, String, Int)]

functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)   


fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

func x = Cons x (Cons 9 Nil)

-- flatMap func values

main :: IO ()
main = do
  quickBatch (applicative trigger)
