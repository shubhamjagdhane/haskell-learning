module Moi where

newtype Moi s a   = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  -- fmap :: (a -> b) -> Moi s a -> Moi s b
  -- fmap :: (a -> b) -> (s -> (a, s)) -> (s -> (b, s))

  fmap f (Moi g) = Moi $ \s -> let (a, s') = g s 
                               in (f a, s')

instance Applicative (Moi s) where
  -- pure :: a -> Moi s a
  pure a =  Moi $ \s -> (a, s)
  -- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  -- (<*>) :: (s -> (a -> b, s)) -> (s -> (a, s)) -> (s -> (b, s))
  Moi f <*> Moi g = Moi $ \s -> let (func, s')  = f s
                                    (a, s'') = g s'
                                in  (func a, s'')

instance Monad (Moi s) where
  -- (>>=) :: (s -> (a,s)) -> (a -> s -> (b,s)) -> (s -> (b,s))
  f >>= g = Moi $ \s -> let (a, s') = runMoi f s
                        in runMoi (g a) s'
