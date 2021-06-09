{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Type constructors are used only at the type level, in type signatures and typeclass declarations and instances.

-- Kinds are the types of types, or types one level up

data Doggies a =
  Husky a
  | Mastiff a
  deriving (Eq, Show)


-- types are static and resolve at compile time  
-- Data are what we’re working with at runtime
-- type Constructor = data Contructor
-- when data constructors take arguments, those arguments refer to other types

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = AirIndia | Emirates | Lufthansa deriving (Eq, Show) 


data Vehicle = Car Manufacturer Price |
               Plane Airline
               deriving (Eq, Show)

-- from the above Vehicle datatype
-- Vehicle is called as type constructor
-- Car, Plane are called as data constructor
-- Manufacturer, Price and Airline are called as type argument


-- Arity refers to the number of arguments a function or contructor takes

-- A function that takes no arguments is called nullary, where nullary is a contraction of “null” and “-ary”.
-- so do nullary, unary, binary so on

-- data constructors that take more than one argument are called products

-- The difference between newtype and the type it contains is gone by the time the compiler generates the code


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where 
  tooMany n = n > 42


-- the newtype declaration will allow you to define a custom instance
newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- instance classname type/data name where
{-
instance TooMany Goats where
  tooMany (Goats n) = tooMany n
-}


-- records

data Person = Person { name :: String
                     , age  :: Int }
                     deriving (Eq, Show)

