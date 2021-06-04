module Learned where

-- this won't work due to the type error
-- divideThenAdd :: Num a => a -> a -> a
-- divideThenAdd x y = (x / y) + 1

divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1

f :: (Num a, Fractional a) => a -> a -> a
f x y = (x / y) + 1

data Mood = Blah
-- I don't understand the meaning of this
instance Show Mood where show _ = "Blah"


-- instances are dispatched by type


-- a typeclass defines a set of functions and/or values
-- types have instances of that typeclass
-- the instances specify the ways that type uses the functions of the typeclass


class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

newtype Age =
  Age Integer
  deriving (Eq, Show)

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65  

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1988  


data Trivial             = Trivial'  
-- type constructor       data constructor/value
instance Eq Trivial where
  Trivial' == Trivial' = True

-- Day of weeks
data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
--  deriving (Ord, Show) -- added in Ord section

data Date =
  Date DayOfWeek Int


instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False  

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') = 
       weekday == weekday' && dayOfMonth == dayOfMonth'

-- identity
data Identity a =
  Identity a

instance (Eq a) => Eq (Identity a) where
  (==) (Identity v) (Identity v') = (v == v')


 -- Ord 
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _ Fri   = LT
  compare _ _     = EQ

-- Gimme more operations
add :: (Num a) => a -> (a -> a) 
add = \a -> \b -> a + b

addWeird :: (Num a, Ord a) => a -> (a -> a)
addWeird = \x -> \y -> if x > 1 -- you can use == Ord implies Eq
                       then x + y
                       else x


-- Ord implies Eq
check' :: (Eq a) => a -> (a -> Bool)
check' = \a -> \a'->  a == a'

-- Eq is superclass of Ord, meas Ord has the == property of Eq
check'' :: (Ord a) => a -> (a -> Ordering)
check'' = \a -> \a'->  compare a a'


-- concrete types

-- add1 :: a -> (a->a) because not all type has + property, you can use Num a as used in the above add
add' :: Int -> (Int->Int) -- works because Int has typeclasses Num, Eq, Ord
add' = \a -> \b-> a + b

{-

class Num a => Fractional a where

Here the typeclass Fractional inherits from Num. We could also say that Num is a superclass of Fractional.


An instance is the definition of how a typeclass should work for a given type. Instances are unique for a given combination of typeclass and type.

-} 
