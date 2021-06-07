import Data.Char

articles :: String -> Bool
articles "the" = False
articles "an" = False
articles "a" = False
articles _ = True

myFilter xs = filter (\x -> articles x) (split ' ' xs)

split :: Char -> [Char] -> [[Char]]
split c [] = []
split c xs = reverse (goSplit c xs "" [])

goSplit c [] word ans = (reverse word) : ans
goSplit c (x:xs) word ans
  | x == ' '  = goSplit c xs "" ((reverse word) : ans)
  | otherwise = goSplit c xs (x : word) ans



-- Data.Char
gimmeUppercase :: String -> String
gimmeUppercase xs = filter (\x-> isUpper x) xs

makeInitialUppercase :: String -> String
makeInitialUppercase (x:xs) = toUpper x : xs

convertUpper :: String -> String
convertUpper xs = map toUpper xs


-- myany
myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs)
  | f x = True
  | otherwise = myAny f xs


squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f xs =  concat $ map f xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy f (x: []) = Nothing
myMaximumBy f (x:xs) = Just $ goMyMaximum f xs x

goMyMaximum f [] x = x
goMyMaximum f (y:ys) x = case f x y of
                          LT -> goMyMaximum f ys y
                          GT -> goMyMaximum f ys x


myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMinimumBy f (x:[]) = Nothing
myMinimumBy f (x:xs) = Just $ goMyMinimum f xs x

goMyMinimum f [] x = x
goMyMinimum f (y:ys) x = case f x y of
                          LT -> goMyMinimum f ys x
                          GT -> goMyMinimum f ys y

myMaximum :: (Ord a) => [a] -> Maybe a
myMaximum [] = Nothing
myMaximum (x:xs) = Just $ goMaximum xs x

goMaximum :: (Ord a) => [a] -> a -> a
goMaximum [] y = y
goMaximum (x:xs) y = case x > y of
                      True -> goMaximum xs x
                      False -> goMaximum xs y
