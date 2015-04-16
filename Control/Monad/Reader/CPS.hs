{-# LANGUAGE Trustworthy, Rank2Types, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Control.Monad.Reader.CPS where
import Control.Monad.Reader.Class
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans

newtype ReaderT r m a = ReaderT { unReaderT :: forall b. r -> (a -> m b) -> m b }

runReaderT :: Monad m => ReaderT r m a -> r -> m a
runReaderT m r = unReaderT m r return
{-# INLINE runReaderT #-}

mapReaderT :: (Monad m, Monad n) => (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT t m = ReaderT $ \r c -> t (unReaderT m r return) >>= c

instance Functor (ReaderT r m) where
    fmap f m = ReaderT $ \r c -> unReaderT m r (c . f)
    {-# INLINABLE fmap #-}

instance Applicative (ReaderT r m) where
    pure x = ReaderT $ \_ c -> c x
    {-# INLINABLE pure #-}
    mf <*> ma = ReaderT $ \r c -> unReaderT mf r $ \f -> unReaderT ma r (c . f)
    {-# INLINABLE (<*>) #-}

instance Monad (ReaderT r m) where
    return x = ReaderT $ \_ c -> c x
    {-# INLINABLE return #-}
    m >>= k = ReaderT $ \r c -> unReaderT m r (\a -> unReaderT (k a) r c)
    {-# INLINABLE (>>=) #-}

instance MonadReader r (ReaderT r m) where
    ask = ReaderT $ \r c -> c r
    {-# INLINABLE ask #-}
    local f m = ReaderT $ \r c -> unReaderT m (f r) c
    {-# INLINABLE local #-}
    reader f = ReaderT $ \r c -> c (f r)
    {-# INLINABLE reader #-}

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ const (m >>=)
    {-# INLINABLE lift #-}

type Reader r = ReaderT r Identity

runReader :: Reader r a -> r -> a
runReader m r = runIdentity (runReaderT m r)
{-# INLINABLE runReader #-}
