{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , TupleSections
           , UndecidableInstances
           #-}

import Control.Applicative
import Control.Monad

newtype Identity a = Identity { runIdentity :: a } 
instance Functor Identity where fmap f (Identity a) = Identity $ f a
instance Applicative Identity where pure a = Identity a
                                    Identity f <*> Identity a = Identity $ f a
instance Monad Identity where return = pure
                              Identity u >>= phi = phi u

class MonadTrans t where
  lift :: Monad m => m a -> t m a


class Monad m => MonadError e m | m -> e where
  throw :: e -> m a
  catch :: m a -> (e -> m a) -> m a


newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance MonadTrans (ExceptT e) where
  lift u = ExceptT (liftM Right u)


instance Functor m => Functor (ExceptT e m) where
  fmap f (ExceptT u) = ExceptT $ fmap (fmap f) u

instance Applicative m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . Right
  ExceptT alpha <*> ExceptT u = ExceptT ((<*>) <$> alpha <*> u)

instance Monad m => Monad (ExceptT e m) where
  return = ExceptT . return . Right 
  ExceptT u >>= phi = ExceptT $ do x <- u
                                   case x of
                                        Left e -> return $ Left e
                                        Right a -> runExceptT . phi $ a

instance Monad m => MonadError e (ExceptT e m) where
  throw = ExceptT . return . Left
  catch (ExceptT u) h = ExceptT $ u >>= handle
    where handle (Left e) = runExceptT $ h e
          handle r = return r




class Monad m => MonadState s m | m -> s where
  get :: m s
  get = state $ \s -> (s,s)

  put :: s -> m ()
  put s = state $ const ((),s)

  state :: (s -> (a,s)) -> m a
  state phi = do s <- get
                 let (a,s') = phi s
                 put s'
                 return a

modify :: MonadState s m => (s -> s) -> m ()
modify f = liftM f get >>= put

gets :: MonadState s m => (s -> a) -> m a
gets proj = liftM proj get


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift u = StateT $ \s -> liftM (,s) u

instance Functor m => Functor (StateT s m) where
  fmap f (StateT u) = StateT $ fmap (\(a,s') -> (f a, s')) . u

instance (Functor m, Monad m) => Applicative (StateT s m) where
  pure a = StateT $ return . (a,)
  StateT alpha <*> StateT u 
    = StateT $ \s -> do (f,s') <- alpha s
                        (a,s'') <- u s'
                        return (f a, s'')

instance Monad m => Monad (StateT s m) where
  return a = StateT $ return . (a,)
  StateT u >>= phi 
    = StateT $ \s -> do (a,s') <- u s
                        runStateT (phi a) s'

instance Monad m => MonadState s (StateT s m) where
  state phi = StateT $ return . phi

instance MonadError e m => MonadError e (StateT s m) where
  throw = lift . throw
  StateT phi `catch` alpha = 
    StateT $ \s -> phi s `catch` alpha
  


type Stack a = [a]

data RuntimeError = DivZero
                  | Overflow
                  | Imaginary
                  | StackUnderflow

numbers :: StateT (Stack Int) (ExceptT RuntimeError Identity) Int
numbers = do modify (3:)
             modify (5:)
             s <- get
             (case s of 
                   (a:b:as) -> return $ a + b
                   _ -> throw StackUnderflow )
               `catch` (\e -> return 0)


