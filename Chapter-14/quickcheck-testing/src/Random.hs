import Test.Hspec
import Test.QuickCheck

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genChar :: Gen Char
genChar = elements ['a'..'z']


genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)