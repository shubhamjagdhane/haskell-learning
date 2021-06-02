-- exercise
ans1 :: [Char] -> [Char]
ans1 x = x

ans2 :: [Char] -> Char
ans2 x = x !! 4


ans3 :: [Char] -> [Char]
ans3 x = drop 9 x

thirdLetter :: [Char] -> Char
thirdLetter x = x !! 2 


letterIndex :: Int -> Char
letterIndex n 
    | n>0 =  "Curry is awesome!" !! n
    | otherwise = "Curry is awesome!" !! (negate n)


-- shall not reverse the all generic string
rvrs :: String -> String
rvrs x = drop 9 x ++ drop 5 (take 9 x) ++ take 5 x


