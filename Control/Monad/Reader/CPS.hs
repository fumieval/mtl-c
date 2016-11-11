{-# LANGUAGE Trustworthy, Rank2Types, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
module Control.Monad.Reader.CPS (ReaderT(..)
    , runReaderT
    , mapReaderT
    , Reader
    , runReader
    , module Control.Monad.Reader.Class) where
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.Writer.Class

newtype ReaderT r m a = ReaderT { unReaderT :: forall b. r -> (a -> m b) -> m b }

runReaderT :: Monad m => ReaderT r m a -> r -> m a
runReaderT m r = unReaderT m r return
{-# INLINABLE runReaderT #-}

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
    m *> n = ReaderT $ \r c -> unReaderT m r (\_ -> unReaderT n r c)
    {-# INLINABLE (*>) #-}

instance Monad (ReaderT r m) where
    return x = ReaderT $ \_ c -> c x
    {-# INLINABLE return #-}
    m >>= k = ReaderT $ \r c -> unReaderT m r (\a -> unReaderT (k a) r c)
    {-# INLINABLE (>>=) #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}

instance MonadReader r (ReaderT r m) where
    ask = ReaderT $ \r c -> c r
    {-# INLINABLE ask #-}
    local f m = ReaderT $ \r c -> unReaderT m (f r) c
    {-# INLINABLE local #-}
    reader f = ReaderT $ \r c -> c (f r)
    {-# INLINABLE reader #-}

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO
    {-# INLINABLE liftIO #-}

instance MonadState s m => MonadState s (ReaderT r m) where
    get = lift get
    {-# INLINABLE get #-}
    put = lift . put
    {-# INLINABLE put #-}

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ const (m >>=)
    {-# INLINABLE lift #-}

instance MonadError e m => MonadError e (ReaderT r m) where
    throwError = lift . throwError
    {-# INLINABLE throwError #-}

    catchError m h = ReaderT $ \r c -> catchError (unReaderT m r return) (\e -> runReaderT (h e) r) >>= c
    {-# INLINABLE catchError #-}

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
    tell = lift . tell
    listen m = ReaderT $ \r c -> listen (runReaderT m r) >>= c
    pass m = ReaderT $ \r c -> pass (runReaderT m r) >>= c

type Reader r = ReaderT r Identity

runReader :: Reader r a -> r -> a
runReader m r = runIdentity (runReaderT m r)
{-# INLINE runReader #-}
