{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Reader.Class
import Control.Monad.Reader as Normal
import Control.Monad.Reader.CPS as CPS

import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Control.Monad.Writer.CPS as CPS
import Data.Monoid

import Control.Monad.State.Class
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.State.CPS as CPS

import Control.Monad.Except
import Control.Monad.Except as Normal
import Control.Monad.Except.CPS as CPS

import Criterion.Main

import Data.IORef

askTest :: MonadReader Int m => (m () -> Int -> t) -> Int -> t
askTest f n = f (replicateM_ n ask) 42

tellTest :: MonadWriter (Sum Int) m => (m () -> t) -> Int -> t
tellTest f n = f (forM_ [1..n] $ tell . Sum)

modifyTest :: MonadState Int m => (m () -> Int -> t) -> Int -> t
modifyTest f n = f (forM_ [1..n] $ \i -> modify (+ i)) 0

exceptTest :: MonadError (Product Int) m => (m () -> t) -> Int -> t
exceptTest f n = f (forM_ [1..n] $ \i -> (pure i >>= \i' -> throwError (Product i') >> pure i')
                                          `catchError` (\(Product e) -> pure (e*e)))

main = do
  ref <- newIORef 10000
  defaultMain
    [ bgroup "State" [
       bench "CPS"       $ nfIO $ readIORef ref >>= modifyTest CPS.runStateT
       , bench "Strict"  $ nfIO $ readIORef ref >>= modifyTest Strict.runStateT
       , bench "Lazy"    $ nfIO $ readIORef ref >>= modifyTest Lazy.runStateT
       ]
    , bgroup "Writer" [
       bench "CPS"       $ nfIO $ readIORef ref >>= tellTest CPS.runWriterT
       , bench "Strict"  $ nfIO $ readIORef ref >>= tellTest Strict.runWriterT
       , bench "Lazy"    $ nfIO $ readIORef ref >>= tellTest Lazy.runWriterT
       ]
    , bgroup "Reader" [
       bench "CPS"       $ nfIO $ readIORef ref >>= askTest CPS.runReaderT
       , bench "Normal"  $ nfIO $ readIORef ref >>= askTest Normal.runReaderT
       ]
    , bgroup "Except"
        [ bench "CPS"    $ nfIO $ readIORef ref >>= exceptTest CPS.runExceptT
        , bench "Normal" $ nfIO $ readIORef ref >>= exceptTest Normal.runExceptT
        ]
    ]
