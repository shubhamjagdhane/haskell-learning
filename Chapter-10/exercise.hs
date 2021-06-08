import Data.Time
-- understanding flodr
-- 5
 
a = foldr (++) "" ["woot", "WOOT", "woot"]
b = foldr max ' ' "fear is the little death"
c = foldr (&&) True [False, True]
d = foldr (||) True [False, True] -- always returns True
e = foldl ((++) . show) "" ["I'm", "Learning", "Haskell"]
f = foldr const 0 [1..5]
g = foldr const ' ' "tacos"
h = foldl (flip const) ' ' "burritos"
i = foldl (flip const) 0 [1..5]


data DatabaseItem = 
  DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]

theDatabase = [ 
  DbDate (UTCTime(fromGregorian 1911 5 1)(secondsToDiffTime 34123)), 
  DbNumber 9001, 
  DbString "Hello, world!", 
  DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = giveMeDbDate . filter isDate $xs

giveMeDbDate :: [DatabaseItem] -> [UTCTime]
giveMeDbDate [] = []
giveMeDbDate (x : xs) = getDbDate x : giveMeDbDate xs

getDbDate :: DatabaseItem -> UTCTime
getDbDate (DbDate x) = x

isDate :: DatabaseItem -> Bool
isDate (DbDate x) = True
isDate _          = False

-- 2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = giveMeDbNumber . filter isDbNumber $xs

giveMeDbNumber :: [DatabaseItem] -> [Integer]
giveMeDbNumber [] = []
giveMeDbNumber (x : xs) = getDbNumber x : giveMeDbNumber xs

getDbNumber :: DatabaseItem -> Integer
getDbNumber (DbNumber x) = x

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber x) = True
isDbNumber _          = False

-- 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = maximum . giveMeDbDate . filter isDate $xs

-- 4 
sumDb :: [DatabaseItem] -> Integer
sumDb xs = sum . giveMeDbNumber . filter isDbNumber $ xs

-- 5

avgDb :: [DatabaseItem] -> Double
avgDb xs = getAverage (giveMeDbNumber . filter isDbNumber $ xs)

getAverage xs = (/) (fromIntegral (sum xs)) (fromIntegral (length xs))


-- Scans Exercise
fibs = 1 : scanl (+) 1 fibs

fibf20 = take 20 fibs 

--fibless100 = go fibs

go (x:xs) = case x <= 100 of
              True -> x : go xs
              False -> []

-- fact using scnal
fact = scanl (*) 1 [2..] 


-- Chapter exercise
stops = "pbtdkg"
vowels = "aeiou"

words = [(x, y, z) | x <- stops, y <- vowels, z <- vowels]
startsWithP = [(x, y, z) | x <- stops, y <- vowels, z <- vowels, x == 'p']

-- rewriting with fold(r/l)

myAnd :: [Bool] -> Bool
myAnd xs = foldr (&&) True xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr ((||) . f) False xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem x xs = foldr ((||) . (== x)) False xs

myReverse :: [a] -> [a]
myReverse xs = foldl (flip (:)) [] xs

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr ((:) . f) [] xs

