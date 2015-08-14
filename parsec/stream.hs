{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           #-}

class Monad m => Stream s m t | s -> t where
  uncons :: s -> m (Maybe (t, s))

instance Monad m => Stream [t] m t where
  uncons [] = return Nothing
  uncons (t:ts) = return . Just $ (t,ts)


