module Chapter4 where

-- length function
myLen :: [a] -> Integer
myLen [] = 0
myLen (x:xs) = 1 + myLen xs

-- palindrome function
isPalindrome :: [Char] -> Bool
isPalindrome x = isPalindrome' (map toLower x)

isPalindrome' :: [Char] -> Bool
isPalindrome' = \x -> x == reverse x

uppercase = ['A'..'Z']
lowercase = ['a'..'z']

toLower :: Char -> Char
toLower = \c -> if isUpper(c) then lowercase !! (check uppercase c) else c

isUpper :: Char -> Bool
isUpper c = isUpper' c uppercase

isUpper' :: Char -> [Char] -> Bool
isUpper' c [] = False
isUpper' c (x:xs) | x == c = True
                  | otherwise = isUpper' c xs

check [] c = 0
check (x:xs) c = check' c (x:xs) 0

check' c [] index = 0
check' c (x:xs) index = if c == x then index else check' c xs index+1


-- absolute value
myAbs :: Integer -> Integer
myAbs = \n -> if n < 0 then negate n else n


-- tuples
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f = \first -> \second ->((snd first, snd second), (fst first, fst second))
