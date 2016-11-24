{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module Control.Monad.Except.CPS
   where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Monad.Trans
import Control.Monad.Identity

#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith))
#endif

import Control.Applicative

import Data.Monoid

newtype ExceptT e m a = ExceptT (forall r. (e -> m r) -> (a -> m r) -> m r)

unExceptT :: ExceptT e m a -> (e -> m r) -> (a -> m r) -> m r
unExceptT (ExceptT m) = m
{-# INLINE unExceptT #-}

type Except e a = ExceptT e Identity a

instance Functor (ExceptT e m) where
  fmap f (ExceptT m) = ExceptT $ \err suc -> m err (suc . f)
  {-# INLINE fmap #-}


instance Applicative (ExceptT e m) where
  pure a = ExceptT $ \_err suc -> suc a
  {-# INLINE pure #-}
  ExceptT mf <*> ExceptT ma = ExceptT $ \err suc -> mf err $ \f -> ma err (suc . f)
  {-# INLINE (<*>) #-}
  ExceptT ma *> ExceptT mb = ExceptT $ \err suc -> ma err $ \_a -> mb err suc
  {-# INLINE (*>) #-}


instance Monad m => Monad (ExceptT e m) where
  return = pure
  {-# INLINE return #-}
  ExceptT ma >>= f = ExceptT $ \err suc -> ma err $ \a -> unExceptT (f a) err suc
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}
  fail str = ExceptT $ \_ _ -> fail str
  {-# INLINE fail #-}

instance Monad m => MonadError e (ExceptT e m) where
  throwError e = ExceptT $ \err _suc -> err e
  {-# INLINE throwError #-}
  catchError (ExceptT ma) err' = ExceptT $ \err suc -> ma (\e -> unExceptT (err' e) err suc) suc
  {-# INLINE catchError #-}

instance MonadTrans (ExceptT e) where
  lift m = ExceptT $ \err suc -> m >>= suc
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO io = ExceptT $ \err suc -> liftIO io >>= suc
  {-# INLINE liftIO #-}

instance MonadReader r m => MonadReader r (ExceptT e m) where
  ask = lift ask
  {-# INLINE ask #-}
  local f m = ExceptT $ \err suc -> local f (unExceptT m err suc)
  {-# INLINE local #-}

instance MonadWriter w m => MonadWriter w (ExceptT e m) where
  tell a = lift (tell a)
  {-# INLINE tell #-}
  listen m = ExceptT $ unExceptT (listen m)
  {-# INLINE listen #-}
  pass m = ExceptT $ unExceptT (pass m)
  {-# INLINE pass #-}

instance MonadState s m => MonadState s (ExceptT e m) where
  get = lift get
  {-# INLINE get #-}
  put a = lift (put a)
  {-# INLINE put #-}
  state f = lift (state f)
  {-# INLINE state #-}

instance (Functor m, Monad m, Monoid e) => Alternative (ExceptT e m) where
  ma <|> mb = ExceptT $ \err suc -> unExceptT ma (\e -> unExceptT mb (err . (e<>)) suc) suc
  {-# INLINE (<|>) #-}
  empty = ExceptT $ \err suc -> err mempty
  {-# INLINE empty #-}

instance (Monad m, Monoid e) => MonadPlus (ExceptT e m) where
  mplus ma mb = ExceptT $ \err suc -> unExceptT ma (\e -> unExceptT mb (err . (e<>)) suc) suc
  {-# INLINE mplus #-}
  mzero = ExceptT $ \err suc -> err mempty
  {-# INLINE mzero #-}


instance (Applicative f,Foldable f) => Foldable (ExceptT e f) where
  foldMap f m = foldMap (either (const mempty) f) (runExceptT m)

#if MIN_VERSION_base(4,4,0)
instance (MonadZip m) => MonadZip (ExceptT e m) where
    mzipWith f (ExceptT ma) (ExceptT mb) = ExceptT $ \err suc ->
      ma err $ \a ->
      mb err $ \b ->
      suc (f a b)
    {-# INLINE mzipWith #-}
#endif


#if MIN_VERSION_base(4,9,0)
instance Fail.MonadFail m => Fail.MonadFail (ExceptT e m) where
  fail x = ExceptT $ \err suc -> fail x
#endif

runExceptT :: Applicative m => ExceptT e m a -> m (Either e a)
runExceptT m = unExceptT m (pure . Left) (pure . Right)
{-# INLINE runExceptT #-}


mapExceptT :: (Applicative m, Monad n) => (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT f m = ExceptT $ \err suc -> either err suc =<< f (runExceptT m)
{-# INLINE mapExceptT #-}

withExceptT :: (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f m = ExceptT $ \ err suc -> unExceptT m (err . f) suc
{-# INLINE withExceptT #-}

runExcept :: Except e a -> Either e a
runExcept m = runIdentity (runExceptT m)
{-# INLINE runExcept #-}


liftEither :: Either e a -> ExceptT e m a
liftEither e = ExceptT $ \err suc ->  either err suc e
{-# INLINE liftEither #-}

mapExcept :: (Either e a -> Either e' b) -> Except e a -> Except e' b
mapExcept f m = liftEither $ f (runExcept m)
{-# INLINE mapExcept #-}

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f m = ExceptT $ \ err suc -> unExceptT m (err . f) suc
{-# INLINE withExcept #-}
