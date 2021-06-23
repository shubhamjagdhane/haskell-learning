module Main where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorComposition :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorComposition f g x = fmap g (fmap f x) == fmap (g .f) x

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

{-
λ> type IntToInt = Fun Int Int
λ> type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
λ> quickCheck (functorCompose' :: IntFC)
-}

main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x::[Int])
  quickCheck $ \x -> functorComposition (+1) (*2) (x::[Int])

  -- for Identity
  putStrLn "Identity testing"
  quickCheck $ \x -> functorIdentity (x ::(Identity Int))
  quickCheck $ \x -> functorIdentity (x ::(Identity String))
  quickCheck $ \x -> functorComposition (+1) (*2) (unIdentity (x::[Identity Int]))
  
  -- for Pair
  putStrLn "Pair testing"
  quickCheck $ \x -> functorIdentity (x ::(Pair Int))
  quickCheck $ \x -> functorComposition (+1) (*2) (unPair (x::[Pair Int]))

  -- for Two
  putStrLn "Two testing"
  quickCheck $ \x -> functorIdentity (x :: (Two Int String))
  quickCheck $ \x -> functorIdentity (x :: (Two Int Int))

  -- for Possibly
  putStrLn "Possibly testing"
  quickCheck $ \x -> functorIdentity (x :: (Possibly Int))

  -- for TalkToMe
  putStrLn "TalkTome testing"
  -- quickCheck $ \x -> functorIdentity (x :: (TalkToMe Int))

-- Identity
newtype Identity a = Identity a deriving (Show, Eq)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do genIdentity

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  a <- arbitrary
  elements [Identity a]

unIdentity :: [Identity a] -> [a]
unIdentity [] = []
unIdentity (Identity x: xs) = x : unIdentity xs

-- Pair
data Pair a = Pair a a deriving (Show, Eq)
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do genPair

unPair :: [Pair a] -> [a]
unPair [] = []
unPair (Pair x y:xs) = x : y : unPair xs

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
  a <- arbitrary
  elements [Pair a a]

-- Two
data Two a b = Two a b deriving (Show, Eq)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do genTwo

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
    a <- arbitrary 
    b <- arbitrary 
    elements [Two a b]


-- Possibly (it is a Maybe datatype with Possibly name)
data Possibly a = LolNope | Yeppers a deriving (Show, Eq)    

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = frequency [(1, return LolNope), (3, fmap Yeppers arbitrary)]


-- TalkToMe
data TalkToMe a = Halt | Print String a | Read a (String -> a)


instance Functor TalkToMe where
  fmap _ Halt         = Halt
  fmap f (Print xs x) = Print xs (f x)
  fmap f (Read a g)   = Read (f a) (f . g)

-- testing remaining for TalkToMde

instance (Arbitrary a) => Arbitrary (TalkToMe a) where
  arbitrary = 
    frequency [
          (1, return Halt), 
          (3, fmap (Print "show") arbitrary)
      ]

genString ::Gen String
genString = do
  a <- arbitrary
  elements [a]

