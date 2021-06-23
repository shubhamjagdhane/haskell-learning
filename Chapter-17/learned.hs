{-

[(*2), (*3)] <*> [1..5]
Just (*2) <*> Just 10
("Woo", (+1)) <*> (" Hoo!", 0)
fmap (2^) [1..3] => [2, 4, 8]
fmap (^2) [1..3] => [1, 4, 9]
lookup 3 [(3, "hello")] => Just "hello"
fmap c $ Data.Map.lookup 3 (fromList [(3, "hello")])



-}

import Control.Applicative
f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]
h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

-- (++) <$> f 3 <*> g 7 => Just "hellosup?"

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
  then Nothing
  else Just s

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a

mkPerson :: String -> String -> Maybe Person
mkPerson n a = 
  Person <$> mkName n <*> mkAddress a

data Cow = Cow { 
    name  :: String 
  , age :: Int 
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe  String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative age 
  | age < 0   = Nothing 
  | otherwise = Just age

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
  Cow <$> noEmpty name'
  <*> noNegative age'
  <*> noNegative weight' 

{-
Applicative Laws:

1. Identity

2. Composition
The result of composing our functions first and 
then applying them and the result of applying the functions
first then composing them should be the same

3. Homomorphism
pure f <*> pure x = pure (f x)

4. Interchange
u <*> pure y = pure ($ y) <*> u

-}