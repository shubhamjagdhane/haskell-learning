module Chapter7 where
-- A value that can be use as argument to a function is a first-class value.

myNum :: Integer
myNum = 1

myVal f = f + myNum

bindExp :: Integer -> String
bindExp x = let y = 5 in 
            "the integer was: " ++ show x 
            ++ " and y was: " ++ show y


-- concept of shadowing
bindExp' :: Integer -> String
bindExp' x = let x = 10; y = 5 in
             "the integer was: " ++ show x
             ++ " and y was: " ++ show y


-- pattern matching

-- Patterns are matched against values, or data constructors, not types


newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisterUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisterUser (Username name) 
          (AccountNumber accNum)) = 
          putStrLn $ name ++ " " ++ show accNum


data WherePenguinsLive =
  Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show) 


data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)  


isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives


humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos


galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False


f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

pal xs =
  case xs == reverse xs of
  True -> "yes"
  False -> "no"

-- we can reuse the value of y if require
pal' xs =
  case y of
  True -> "yes"
  False -> "no"
  where y = xs == reverse xs  
