module Addition where
import Data.Char

newtype MyInteger = MyInteger Integer deriving Show
newtype MyDouble = MyDouble Double deriving Show

newtype AddType = AddType (Either Integer Double) 
instance Show AddType where
  show (AddType (Left x))  = show x
  show (AddType (Right x)) = show x  


data FinalType = Success AddType | Failure

instance Show FinalType where
   show (Success x) = show x
   show Failure   = "Please enter only digits!"


add :: String -> String -> FinalType
add x y
  | isAlphaPresent x || isAlphaPresent y = Failure
  | isDotPresent x || isDotPresent y     = Success $ AddType $ Right $ (read x :: Double) + (read y :: Double)
  | otherwise                            = Success $ AddType $ Left $ (read x :: Integer) + (read y :: Integer)


isAlphaPresent :: String -> Bool
isAlphaPresent [] = False
isAlphaPresent (x:xs)
  | isAlpha x = True
  | otherwise = isAlphaPresent xs

isDotPresent :: String -> Bool
isDotPresent [] = False
isDotPresent (x:xs) 
  | x == '.' = True
  | otherwise = isDotPresent xs