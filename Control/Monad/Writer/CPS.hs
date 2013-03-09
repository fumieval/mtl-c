{-# LANGUAGE Rank2Types, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Control.Monad.Writer.CPS where

import Control.Monad.Writer.Class
import Control.Applicative
import Control.Monad.Identity
import Data.Monoid

newtype WriterT w m a = WriterT { unWriterT :: forall r. (a -> w -> m r) -> m r }

runWriterT :: Monad m => WriterT w m a -> m (a, w)
runWriterT m = unWriterT m (\a w -> return (a, w))

instance Functor (WriterT w m) where
    fmap f m = WriterT $ \c -> unWriterT m (\a w -> c (f a) w)

instance Monoid w => Applicative (WriterT w m) where
    pure x = WriterT $ \c -> c x mempty
    mf <*> ma = WriterT $ \c -> unWriterT mf (\f w -> unWriterT ma (\a w' -> c (f a) (mappend w w')))

instance Monoid w => Monad (WriterT w m) where
    return x = WriterT $ \c -> c x mempty
    m >>= k = WriterT $ \c -> unWriterT m (\a w -> unWriterT (k a) (\b w' -> c b (mappend w w')))

instance Monoid w => MonadWriter w (WriterT w m) where
    writer (a, w) = WriterT $ \c -> c a w
    tell w = WriterT $ \c -> c () w
    listen m = WriterT $ \c -> unWriterT m (\a w -> c (a, w) w)
    pass m = WriterT $ \c -> unWriterT m (\(a, f) w -> c a (f w))

type Writer w = WriterT w Identity

runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT