module Constant where
import Data.Monoid ( Sum(Sum, getSum) )


-- not completed

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant xs) = Constant xs
   
instance Monoid a => Applicative (Constant a) where
  pure x = undefined 
  (Constant xs) <*> (Constant ys) = Constant xs