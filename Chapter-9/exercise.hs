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

{- not yet solved

--myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = GT
myMaximumBy f (x:y:xs) = case (f x y) of 
                          GT -> myMaximumBy f (x:xs)
                          LT -> myMaximumBy f (y:xs)
-}

