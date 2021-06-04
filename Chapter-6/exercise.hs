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
