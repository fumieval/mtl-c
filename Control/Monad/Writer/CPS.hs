{-# LANGUAGE Trustworthy, Rank2Types, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, BangPatterns #-}
module Control.Monad.Writer.CPS (WriterT(..)
    , runWriterT
    , execWriterT
    , mapWriterT
    , Writer
    , runWriter
    , execWriter
    , module Control.Monad.Writer.Class)where

import Control.Monad.Writer.Class
import Control.Applicative
import Control.Monad.Identity
import Data.Monoid
import Control.Monad.Trans
import Unsafe.Coerce

newtype WriterT w m a = WriterT { unWriterT :: forall r. (a -> w -> m r) -> m r }

runWriterT :: Monad m => WriterT w m a -> m (a, w)
runWriterT m = unWriterT m (\a w -> return (a, w))
{-# INLINABLE runWriterT #-}

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT m = unWriterT m (const return)
{-# INLINABLE execWriterT #-}

mapWriterT :: (Monad m, Monad n) => (m (a, w) -> n (b, w)) -> WriterT w m a -> WriterT w n b
mapWriterT t m = WriterT $ \c -> t (unWriterT m (\a w -> return (a, w))) >>= \(b, w') -> c b w'

instance Functor (WriterT w m) where
    fmap f m = WriterT $ \c -> unWriterT m (c . f)
    {-# INLINABLE fmap #-}

instance Monoid w => Applicative (WriterT w m) where
    pure x = WriterT $ \c -> c x mempty
    {-# INLINABLE pure #-}
    mf <*> ma = WriterT $ \c -> unWriterT mf $ \f !w -> unWriterT ma $ \a !w' -> c (f a) $! mappend w w'
    {-# INLINABLE (<*>) #-}

instance Monoid w => Monad (WriterT w m) where
    return x = WriterT $ \c -> c x mempty
    {-# INLINABLE return #-}
    m >>= k = WriterT $ \c -> unWriterT m $ \a !w -> unWriterT (k a) $ \b !w' -> c b $! mappend w w'
    {-# INLINABLE (>>=) #-}
    m >> n = WriterT $ \c -> unWriterT m $ \_ !w -> unWriterT n $ \b !w' -> c b $! mappend w w'
    {-# INLINABLE (>>) #-}

instance Monoid w => MonadWriter w (WriterT w m) where
    writer (a, w) = WriterT $ \c -> c a w
    {-# INLINABLE writer #-}
    tell w = WriterT $ \c -> c () w
    {-# INLINABLE tell #-}
    listen m = WriterT $ \c -> unWriterT m (\a w -> c (a, w) w)
    {-# INLINABLE listen #-}
    pass m = WriterT $ \c -> unWriterT m (\(a, f) w -> c a (f w))
    {-# INLINABLE pass #-}

instance Monoid w => MonadTrans (WriterT w) where
    lift m = WriterT $ \c -> m >>= \a -> c a mempty
    {-# INLINABLE lift #-}

type Writer w = WriterT w Identity

runWriter :: Writer w a -> (a, w)
runWriter = (unsafeCoerce `asTypeOf` (runIdentity.)) runWriterT
{-# INLINE runWriter #-}

execWriter :: Writer w a -> w
execWriter = (unsafeCoerce `asTypeOf` (runIdentity.)) execWriterT
{-# INLINE execWriter #-}
