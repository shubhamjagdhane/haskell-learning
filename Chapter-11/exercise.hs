{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Volvo | Tata | Audi deriving (Eq, Show)

data Airline = AirIndia | Emirates | Lufthansa deriving (Eq, Show)

data Size = HundredMeters | TwoHundredMeters deriving (Eq, Show)
data Vehicle = Car Manufacturer Price |
               Plane Size Airline 
               deriving (Eq, Show)

myCar    = Car Volvo (Price 14000)
urCar    = Car Tata (Price 16000)
clownCar = Car Audi (Price 20000)
doge     = Plane HundredMeters AirIndia 


-- 2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _       = False


isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False


areCars :: [Vehicle] -> [Bool]
areCars []     = []
areCars (x:xs) = isCar x : areCars xs


-- 3

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car m p) = Just m
getManu _ = Nothing

-- cardinality

data Example = MakeExample deriving (Eq, Show) -- 1 cardinality

-- Logic Goats

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany x = x > 42

instance TooMany (String, Int) where
  tooMany x = snd x > 42

instance TooMany (Int, Int) where
  tooMany x = (fst x + snd x) > 42
{-
instance TooMany (Num a, TooMany a) => (a, a) where
  tooMany x = sum x > 42
-}
