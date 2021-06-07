safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x: []) = Nothing
safeTail (x: xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


articles :: String -> Bool
articles "the" = False
articles "an" = False
articles "a" = False
articles _ = True

myFilter xs = filter (\x -> articles x) (split ' ' xs)

--split :: char -> [Char] -> [Char]
split c [] = []
split c xs = reverse (goSplit c xs "" [])

goSplit c [] word ans = (reverse word) : ans
goSplit c (x:xs) word ans
  | x == ' '  = goSplit c xs "" ((reverse word) : ans)
  | otherwise = goSplit c xs (x : word) ans

