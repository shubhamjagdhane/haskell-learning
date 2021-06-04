module Chapter6 where

-- 1
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn n) (TisAn n') = n == n'


-- 2
data TwoInteger = Two Integer Integer

instance Eq TwoInteger where
  (==) (Two n1 n2) (Two n1' n2') = (n1==n1') && (n2==n2')

-- 3
data StringOrInt =
  TisAnInt Int |
  TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt n) (TisAnInt n') = (n == n')
  (==) (TisAString n) (TisAString n') = (n == n')


-- 4
data Pair a = Pair a a
instance (Eq a) => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = (a==a') && (b==b')

-- 5
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = (a==a') && (b==b')

-- my tuple (not a part of exercise)
data MyTuple a = MyTuple (a, a)
instance (Eq a) => Eq (MyTuple a) where
  (==) (MyTuple a) (MyTuple a') = (a == a')  


-- 6
data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) (ThisOne a) (ThatOne a') = False
  (==) (ThatOne a') (ThisOne a) = False

-- 7
data EitherOr a b = Hello a | GoodBye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (GoodBye a) (GoodBye a') = a == a'
  (==) (Hello a) (GoodBye a') = False
  (==) (GoodBye a) (Hello a') = False


-- chapter exercies

  -- does it typecheck?

x :: (Num a, Show a) => (a -> a)
x  = \blah -> blah + 10

printIt :: IO ()
printIt = putStrLn (show (x 12))


-- 1

data Person = Person Bool -- deriving Show

-- second approach
instance Show  Person where
  show (Person True) = "True" 
  show (Person False) = "False"

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2
data Mood = Blah | Woot deriving (Show, Eq) -- added Eq are we are compairing x with Woot
settleDown :: Mood -> Mood
settleDown x = if x == Woot
               then Blah
               else x

{-
-- 3. If you were able to get settleDown to typecheck:
a) What values are acceptable inputs to that function?
b) What will happen if you try to run settleDown 9? Why?
c) What will happen if you try to run Blah > Woot? Why?

Ans: 
  a) Only Blah and Woot
  b) As settleDown take only Mood and return Mood so it shall gives you type error as it does not have instance of Num for Int
  c) It shall gives you type error as we don't have a Ord instance for our datatype
-}

-- 4
type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "Shubham" "is learning" "Haskell"
s2 = Sentence "Julie" "loves" "dogs"

-- Given datatype declaretion, what we can do?
data Rocks = Rocks String deriving (Eq, Show, Ord)
data Yeah = Yeah Bool deriving (Eq, Show, Ord)
data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)

  -- 1
phew = Papu (Rocks "chases") (Yeah True)

  -- 2
truth = Papu (Rocks "chomsky") (Yeah True)  

  -- 3
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'  

  -- equalityForall (Papu (Rocks "Shubham") (Yeah True)) (Papu (Rocks "Shubham") (Yeah True)) => returns True as both Papu objects are having same values
  -- equalityForall (Papu (Rocks "Shubham") (Yeah True)) (Papu (Rocks "Shubham") (Yeah False)) => retusn False

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

  -- as earlier there wasn't Ord so we have to add Ord from deriving


  -- Match the types

-- 1
i :: Integer
i = 12

-- i :: a => fails

-- 2

-- f :: Num a => a -- not works, need Fractional
-- f = 1.0


-- 9 not able to import sort
-- jung :: Ord a => [a] -> a
-- jung xs = head (sort xs)


-- Type-Kwon-Do Two: Electric Typealoo

  -- 1
chk :: (Eq b) => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == (b)

square x = x * 2

  -- 2 => not working
-- arith :: Num b => (a -> b) -> Integer -> a -> b
-- arith f a1 a2 = (f a2) * (a2+10)



