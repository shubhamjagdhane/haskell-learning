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