module StateT where

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
  -- fmap :: (Functor m) => (a -> b) -> State s m a -> State s m b
  fmap ab sa = StateT $ \s ->
    (\(a, s') -> (ab a, s')) <$> runStateT sa s


instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s) 
-- (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  sab <*> sa = StateT $ \s ->
    let mabs = runStateT sab s
        mas  = runStateT sa s
    in (\ (ab, s') -> \(a, s'') -> (ab a, s'')) <$> mabs <*> mas
    {-
      StateT $ \s -> 
        (\(ab, s') -> (\(a, s'') -> (ab a, s''))
        <$> runStateT sa s')
        =<< runStateT sab s
    -}


instance (Monad m) => Monad (StateT s m) where
  return a = StateT $ \s -> pure (a, s)
  -- (>>=) :: Monad m => StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT f >>= sa = StateT $ \s -> do
    (a, s') <- f s
    runStateT (sa a) s'
