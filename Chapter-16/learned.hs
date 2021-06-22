module Learned where

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap f FixMe   = FixMe
  fmap f (Pls a) = Pls (f a)


data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)  

instance Functor WhoCares where
  fmap _ ItDoesnt         = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a)       = Matter $ f a

-- our instance must follow identity law, else it's not a valid functor  

incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)

showMaybe'' :: Show a => Maybe a -> Maybe String
showMaybe'' = fmap show

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+1)

showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show


newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)
-- the type parameter b is a phantom type

data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)


getInt :: IO Int
getInt = fmap read getLine

-- :set -XRank2Types
type Nat f g = forall a . f a -> g a
-- This'll work
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]
