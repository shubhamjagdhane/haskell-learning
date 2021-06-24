module Exercise where

import Control.Monad (join)

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