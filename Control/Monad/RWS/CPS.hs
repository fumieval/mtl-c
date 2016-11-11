{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}

module Control.Monad.RWS.CPS
    ( -- * The RWST monad transformer
      RWST(..)
    , runRWST
    , evalRWST
    , execRWST

      -- * Re-exports
    , module Control.Monad.RWS.Class
    ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import Control.Monad.Trans
import Data.Monoid

-------------------------------------------------------------------------------
-- The RWST monad transformer

-- | A monad transformer adding reading an environment of type @r@,
-- collecting an output of type @w@ and updating a state of type @s@
-- to an inner monad @m@.
newtype RWST r w s m a = RWST { unRWST :: forall x. r -> s -> w -> (a -> s -> w -> m x) -> m x }

runRWST :: (Monad m, Monoid w) => RWST r w s m a -> r -> s -> m (a, s, w)
runRWST m r s = unRWST m r s mempty $ \a s' w' -> return (a, s', w')

-- | Evaluate a computation with the given initial state and environment,
-- returning the final value and output, discarding the final state.
evalRWST
    :: (Monad m, Monoid w)
    => RWST r w s m a      -- ^ computation to execute
    -> r                   -- ^ initial environment
    -> s                   -- ^ initial value
    -> m (a, w)            -- ^ computation yielding final value and output
evalRWST m r s = unRWST m r s mempty $ \a _ w -> return (a, w)

-- | Evaluate a computation with the given initial state and environment,
-- returning the final state and output, discarding the final value.
execRWST
    :: (Monad m, Monoid w)
    => RWST r w s m a      -- ^ computation to execute
    -> r                   -- ^ initial environment
    -> s                   -- ^ initial value
    -> m (s, w)            -- ^ computation yielding final state and output
execRWST m r s = unRWST m r s mempty $ \_ s' w -> return (s', w)

-------------------------------------------------------------------------------
-- Instances from base

instance Functor (RWST r w s m) where
    fmap f m = RWST $ \ r s w k -> unRWST m r s w $ k . f

instance Applicative (RWST r w s m) where
    pure a = RWST $ \ _ s w k -> k a s w
    RWST mf <*> RWST mx = RWST $ \ r s0 w0 k -> mf r s0 w0 $ \f s1 w1 ->
        mx r s1 w1 $ k . f
    RWST mf *> RWST mx = RWST $ \ r s0 w0 k -> mf r s0 w0 $ \_ s1 w1 ->
        mx r s1 w1 k

instance Monad m => Monad (RWST r w s m) where
    return a = RWST $ \ _ s w k -> k a s w
    m >>= k = RWST $ \ r s0 w0 cont -> unRWST m r s0 w0 $ \a s1 w1 ->
        unRWST (k a) r s1 w1 cont
    (>>) = (*>)
    fail msg = RWST $ \ _ _ _ _ -> fail msg

instance MonadTrans (RWST r w s) where
    lift m = RWST $ \ _ s w k -> m >>= \a -> k a s w

instance MonadIO m => MonadIO (RWST r w s m) where
    liftIO = lift . liftIO

instance (Monoid w, Monad m) => MonadRWS r w s (RWST r w s m)

instance (Monoid w, Monad m) => MonadReader r (RWST r w s m) where
    ask = RWST $ \ r s w k -> k r s w
    local f m = RWST $ \ r s w k -> unRWST m (f r) s w k
    reader f = RWST $ \ r s w k -> k (f r) s w

instance (Monoid w, Monad m) => MonadWriter w (RWST r w s m) where
    writer (a, w) = RWST $ \ _ s w0 k -> k a s (w0 <> w)
    tell w = RWST $ \ _ s w0 k -> k () s (w0 <> w)
    listen m = RWST $ \ r s w0 k -> unRWST m r s mempty
        $ \a s' w1 -> k (a, w1) s' (w0 <> w1)
    pass m = RWST $ \ r s w0 k -> unRWST m r s mempty
        $ \(a, f) s' w1 -> k a s' (w0 <> f w1)

instance (Monad m, Monoid w) => MonadState s (RWST r w s m) where
    get = RWST $ \ _ s w k -> k s s w
    put s = RWST $ \ _ _ w k -> k () s w
    state f = RWST $ \ _ s w k -> case f s of (a,s') -> k a s' w
