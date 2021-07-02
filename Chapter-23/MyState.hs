module MyState where

newtype MyState s a = MyState { runMyState :: s -> (a, s)}

instance Functor (MyState s) where
  -- fmap :: (a -> b) -> f a -> f b
  -- fmap :: (a -> b) -> MyState s a -> MyState s b
  -- fmap :: (a -> b) -> (s -> (a, s)) -> (s -> (b ,s))
  fmap f (MyState g) = MyState $ \s -> let (a, s') = g s
                                       in (f a, s')

instance Applicative (MyState s) where
  pure x = MyState $ \s -> (x, s)
  -- (<*>) :: f (a -> b) -> f a -> f b
  -- (<*>) :: MyState s (a -> b) -> MyState s a -> MyState s b
  -- (<*>) :: (s -> ((a -> b), s)) -> (s -> (a, s)) -> (s -> (b, s))
  (MyState f) <*> (MyState g) = MyState $ \s -> let (func, s') = f s
                                                    (a, s'')   = g s'
                                                in  (func a, s'')

instance Monad (MyState s) where
  return x = MyState $ \s -> (x, s)
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) :: MyState s a -> (a -> MyState s b) -> (MyState s b)
  -- (>>=) :: (s -> (a, s)) ->  (a -> (s -> (b, s))) -> (s -> (b, s))
  f >>= g = MyState $ \ s -> let (a, s') = runMyState f s
                             in runMyState (g a) s'