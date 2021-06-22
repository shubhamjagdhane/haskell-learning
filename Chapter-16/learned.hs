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

