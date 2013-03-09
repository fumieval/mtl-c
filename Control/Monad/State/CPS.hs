{-# LANGUAGE Rank2Types, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Control.Monad.State.CPS where
import Control.Monad.State.Class
import Control.Applicative
import Control.Monad.Identity

newtype StateT s m a = StateT { unStateT :: forall r. s -> (a -> s -> m r) -> m r }

runStateT :: Monad m => StateT s m a -> s -> m (a, s)
runStateT m s = unStateT m s (\a s -> return (a, s))

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = unStateT m s ((return.).const)

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = unStateT m s ((return.).flip const)

instance Functor (StateT s m) where
    fmap f m = StateT $ \s c -> unStateT m s (\a s' -> s' `seq` c (f a) s')

instance Applicative (StateT s m) where
    pure x = StateT $ \s c -> c x s
    mf <*> ma = StateT $ \s c -> unStateT mf s (\f s' -> s' `seq` unStateT ma s' (\a s'' -> c (f a) s''))

instance Monad (StateT s m) where
    return x = StateT $ \s c -> c x s
    m >>= k = StateT $ \s c -> unStateT m s (\a s' -> s' `seq` unStateT (k a) s' c)

instance MonadState s (StateT s m) where
    get = state $ \s -> (s, s)
    put s = state $ \_ -> ((), s)
    state f = StateT $ \s c -> uncurry c (f s)

type State s = StateT s Identity

runState :: State s a -> s -> (a, s)
runState m = runIdentity . runStateT m

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)
