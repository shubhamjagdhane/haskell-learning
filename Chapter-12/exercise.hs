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

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                  Just (a, b') -> a : myUnfoldr f b'
                  Nothing      -> []


betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1 -> unfold for binary tree
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
              Just (a, a', a'') -> Node Leaf (a') Leaf
              Nothing           -> Leaf
  
  
-- 2 -> build tree

treeBuild :: Integer -> BinaryTree Integer
treeBuild 0 = Leaf
treeBuild n = goTreeBuild n 0

goTreeBuild :: Integer -> Integer -> BinaryTree Integer
goTreeBuild n stop = case stop == n of
                      True  -> Leaf
                      False -> Node (goTreeBuild n (stop+1)) stop (goTreeBuild n (stop+1))
