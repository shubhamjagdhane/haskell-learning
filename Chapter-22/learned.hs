module Learned where

{-
when you see or hear the term Reader, it’ll be
referring to the Monad or Applicative instances


Prelude> fmap (+1) (*2) 3

-- Rearranging a little bit syntactically
fmap (+1) (*2) $ 3 = 7
(fmap (+1) (*2)) 3 = 7
(+1) . (*2) $ 3    = 7
(+2) . (*1) $ 2    = 4
fmap (+2) (*1) $ 2 = 4
(+2) `fmap` (*1) $ 2 = 4


(.) :: (b -> c) -> (a -> b) -> (a -> c)
fmap :: Functor f => (a -> b) -> f a -> f b

-- we're going to remove the names of the functions
-- and the typeclass constraint as we can take it for
-- granted from here on out.

:: (b -> c) -> (a -> b) -> (a -> c)
:: (a -> b) -> f a -> f b

-- Changing up the letters without changing the meaning
:: (b -> c) -> (a -> b) -> (a -> c)
:: (b -> c) ->   f b    ->    f c

-- f is ((->) a)
:: (b -> c) -> (a -> b) -> (a -> c)
:: (b -> c) -> ((->) a) b -> ((->) a) c

-- Unroll the prefix notation into infix
:: (b -> c) -> (a -> b) -> (a -> c)
:: (b -> c) -> (a -> b) -> (a -> c)
Bada bing. Functorial lifting for functions.


-}


foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r


-- functional Monad
-- (>>=) :: m    a ->  (a -> m    b) ->  m    b
fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r