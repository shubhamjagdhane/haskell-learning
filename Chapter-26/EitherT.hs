module EitherT where

newtype EitherT e m a =
  EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ma) =
    EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure . pure $ x
  (EitherT fab) <*> (EitherT mma) =
    EitherT $ (<*>)  <$> fab <*> mma

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f =
    EitherT $ do
      v <- ma
      case v of 
        -- m (Either e b)
        -- m e
        Left x  -> return $ Left x
        Right y -> runEitherT (f y)

swapEither :: Either e a -> Either a e
swapEither = either Right Left

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT = EitherT . fmap swapEither . runEitherT

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT x) = do
  v <- x
  case v of 
    Left x  -> f x
    Right y -> g y