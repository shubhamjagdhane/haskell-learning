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
  
