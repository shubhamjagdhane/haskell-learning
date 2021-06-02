module Check where

-- question-1

half :: Fractional a => a -> a
half x = x / 2

square :: Num a => a -> a
square x = x * x

-- question-2
cr :: Double -> Double
cr r = 3.14 * square r
