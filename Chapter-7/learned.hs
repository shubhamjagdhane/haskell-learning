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

-- newtype is different in that it permits only one constructor and only one field.

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


data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)


coderRulesCEOsDrool :: Employee -> Employee -> Ordering
coderRulesCEOsDrool Coder Coder = EQ
coderRulesCEOsDrool Coder _ = GT
coderRulesCEOsDrool _ Coder = LT 
coderRulesCEOsDrool e e' = compare e e'

reportBoss :: Employee -> Employee -> IO()
reportBoss e e' = putStrLn $ show e ++ " is the Boss of " ++ show e'


employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO()
employeeRank f e e' = 
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'    

myAbs :: Integer -> Integer
myAbs x
  | x < 0 = (-x)
  | otherwise = x


bloodNa :: Integer -> String
bloodNa x 
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"


avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100


-- function composition:
-- Function composition is a type of higher-order function that allows us to combine functions such that the result of applying one function gets passed to the next function as argument

temp1 = take 5 . filter odd . enumFrom $ 4
temp2 = take 5 $ filter odd $ enumFrom 4


-- Pointfree style

{-
  (f . g) x = f (g x)
  f . g = \x -> f (g x)
  f . g . h = \x -> f (g (h x))
-}
