module Person where

import Control.Applicative (liftA2)  
import Control.Monad

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)  

data Person =
  Person {
    humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  } deriving (Show, Eq)

data Dog =
  Dog {
    dogsName    :: DogName
  , dogsAddress :: Address
  } deriving (Show, Eq)


pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris = 
  Person (HumanName "Chris Allen")         
         (DogName "Papu")
         (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = 
  Dog <$> dogName <*> address


getDogR' :: Person -> Dog
getDogR' = 
  liftA2 Dog dogName address

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \r -> f (ra r)  

instance Applicative (Reader r) where
  --pure :: a -> Reader r a
  pure a = Reader $ \r -> a
  -- (<*>) :: Reader r (a -> b)  -> Reader r a  -> Reader r b
  (Reader rab) <*> (Reader ra) =  Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  -- (Reader ra) >>= aRb =  Reader $ \r -> aRb (ra r) r -- wrong, working on

getDogReader :: Reader Person Dog
getDogReader = Dog <$> Reader dogName <*> Reader address  

asks :: (r -> a) -> Reader r a
asks = Reader
