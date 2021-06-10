import Data.Char

notThe :: String -> Maybe String
notThe xs
  | length xs == 3 && (xs !! 0 =='t' && xs !! 1 == 'h' && xs !! 2 == 'e') = Nothing
  | otherwise = Just xs


getWords :: String -> [String]
getWords [] = []
getWords xs = reverse $ goGetWords xs "" []

goGetWords :: String -> String -> [String] -> [String]
goGetWords [] word words = (reverse word: words)
goGetWords (x:xs) word words
  | x == ' ' = goGetWords xs "" (reverse word : words)
  | otherwise = goGetWords xs (x:word) words

replaceThe :: String -> String
replaceThe xs = init . goReplaceThe . getWords $ xs


goReplaceThe :: [String] -> String
goReplaceThe [] = ""
goReplaceThe (x:xs)
  |  length x == 3 && toLowercase x == "the" = "a" ++ " " ++ goReplaceThe xs
  | otherwise               = x ++ " " ++ goReplaceThe xs


toLowercase :: String -> String
toLowercase [] = []
toLowercase (x:xs) = toLower x : toLowercase xs



data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ Zero) = 1
natToInteger (Succ (Succ Zero)) = 2


myIterate :: (a -> a) -> a -> [a]
myIterate f n = n : myIterate f (f n)

