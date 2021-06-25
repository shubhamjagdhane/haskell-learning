module Learned where

import Control.Monad (join, (>=>))

{-
bind :: Monad a => (a -> m b) -> m a -> m b
fmap :: Functor f => (a -> b) -> f a -> f b
join :: Monad m => m (m a) -> m a
-}

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x


half ::  Integral a => a -> Maybe a
half x = if even x then Just $ div x 2 else Nothing



-- temp :: Functor f => (a -> b) -> [f a] ->  b
temp :: Functor f => (a -> b) -> [f a] -> [f b]
temp _ [] = []
temp f (x:xs) = fmap f x: temp f xs


binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= putStrLn

-- binding and binding' both are doing the same work

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else []


data Cow = Cow {
    name:: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n 
  | n >= 0 = Just n
  | otherwise = Nothing  

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
      in if n == "Bess" && w > 499
      then Nothing
      else Just c  

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty -> weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' =
  do
    nammy <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)     


mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  \ nammy ->
  noNegative age' >>=
  \ agey ->
  noNegative weight' >>=
  \ weighty ->
  weightCheck (Cow nammy agey weighty)    

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1)
  else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b

  pure (a, b, c)

{-
When we reassociate them, we need to apply f so that g has an
input value of type m a to start the whole thing off. So, we pass in the
argument x via an anonymous function:
m >>= (\x -> f x >>= g)


fmap f . fmap g = fmap (f . g)

e.g fmap (+1) . fmap (*2) $ [1..5] = fmap ((+1) . (*2)) [1..5]


mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join (f <$> (g a))

But using join and fmap together means we can go ahead and just use (>>=).
mcomp'' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp'' f g a = g a >>= f

-}  



sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "