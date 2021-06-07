import Data.List (intersperse)
-- sum of numbers
addUpton :: (Num a, Ord a) => a -> a
addUpton n 
  | n < 0 = negate $ go (-n) 0
  | otherwise = go n 0

go :: (Num a, Eq a) => a -> a -> a
go 0 ans = ans
go x ans = go (x-1) (ans+x)


-- multiply two numbers
mult :: (Integral a) => a -> a -> a
mult x 0 = 0
mult 0 y = 0
mult x y 
  | x > y = x + mult x (y-1)
  | otherwise = y + mult (x-1) y


-- fixing divide by
data DividedResult = Result Integer | DivideByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult 
dividedBy num denom = goDivided num denom 0

goDivided n d count
  | d == 0 = DivideByZero
  | n < d = Result count
  | otherwise = goDivided (n - d) d (count + 1)


-- mc91
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))


-- digit to word
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "Please pass digit only"

-- get digits from a number
digits :: Int -> [Int]
digits n = goDigits n []

goDigits :: (Integral a, Ord a) => a -> [a] -> [a]
goDigits n ans
  | n < 10 = [n] ++ ans
  | otherwise = goDigits (div n 10) ([mod n 10] ++ ans)


wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
