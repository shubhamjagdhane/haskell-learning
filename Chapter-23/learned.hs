module Learned where


import System.Random

sg = mkStdGen 0
tmp = next sg

{-
> snd (next sg)
> let newSg = snd (next sg)
> next newSg
> randomR (0, 3) newSg :: (Int, StdGen)
> randomR (0, 3) newSg :: (Double, StdGen)
-}

type Iso a b = (a -> b, b -> a)
newtype Sum' a = Sum' { getSum' :: a }

sumIsIsomorphicWithItsContents :: Iso a (Sum' a)
sumIsIsomorphicWithItsContents =
  (Sum', getSum')