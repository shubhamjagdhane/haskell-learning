module Main where
import Identity
import Optional
import List
import Tree

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Optional
instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Yep a, Nada]
instance Eq a => EqProp (Optional a) where (=-=) = eq

-- Identity
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary
instance Eq a => EqProp (Identity a) where (=-=) = eq

-- List
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList
genList :: Arbitrary a => Gen (List a)
genList = do
  a <- arbitrary
  elements [Nil, Cons a Nil]
instance (Eq a) => EqProp (List a) where (=-=) = eq

-- Tree
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genTree
genTree :: Arbitrary a => Gen (Tree a)
genTree = do
  a <- arbitrary
  elements [Empty, Leaf a, Node (Leaf a) a (Leaf a), Node (Node (Leaf a) a (Leaf a)) a (Leaf a)]
instance (Eq a) => EqProp (Tree a) where (=-=) = eq

main :: IO ()
main = do
  putStr "\n\t\t---------------- Testing Identity ----------------"
  let identity = undefined :: Identity (Int, Int, [Int])
  quickBatch (traversable identity)

  putStr "\n\t\t---------------- Testing Optional ----------------"
  let optional = undefined :: Optional (Int, Int, [Int])
  quickBatch (traversable optional)

  putStr "\n\t\t---------------- Testing List ----------------"
  let list = undefined :: List (Int, Int, [Int])
  quickBatch (traversable list)

  putStr "\n\t\t---------------- Testing Tree ----------------"
  let tree = undefined :: Tree (Int, Int, [Int])
  quickBatch (traversable tree)
  