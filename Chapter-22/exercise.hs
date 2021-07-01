{-# LANGUAGE InstanceSigs #-}

module Exercise where
import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap 

fmapped :: [Char] -> [Char]
fmapped = cap . rev

tupled :: [Char] -> ([Char], [Char])
tupled xs =  do
  let revString = rev xs
  let capString = fmapped revString
  (revString, capString)

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f xs ys = f <$> xs <*> ys 
