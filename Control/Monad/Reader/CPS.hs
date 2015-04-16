{-# LANGUAGE Rank2Types, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Control.Monad.Reader.CPS where
import Control.Monad.Reader.Class
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans

newtype ReaderT r m a = ReaderT { unReaderT :: forall b. r -> (a -> m b) -> m b }

runReaderT :: Monad m => ReaderT r m a -> r -> m a
runReaderT m r = unReaderT m r return
{-# INLINE runReaderT #-}

instance Functor (ReaderT r m) where
    fmap f m = ReaderT $ \r c -> unReaderT m r (c . f)

instance Applicative (ReaderT r m) where
    pure x = ReaderT $ \_ c -> c x
    mf <*> ma = ReaderT $ \r c -> unReaderT mf r $ \f -> unReaderT ma r (c . f)

instance Monad (ReaderT r m) where
    return x = ReaderT $ \_ c -> c x
    m >>= k = ReaderT $ \r c -> unReaderT m r (\a -> unReaderT (k a) r c)

instance MonadReader r (ReaderT r m) where
    ask = ReaderT $ \r c -> c r
    local f m = ReaderT $ \r c -> unReaderT m (f r) c
    reader f = ReaderT $ \r c -> c (f r)

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ const (m >>=)

type Reader r = ReaderT r Identity

runReader :: Reader r a -> r -> a
runReader m r = runIdentity (runReaderT m r)
