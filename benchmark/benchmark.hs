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

import Criterion.Main

askTest :: MonadReader Int m => (m () -> Int -> t) -> Int -> t
askTest f n = f (replicateM_ n ask) 42

tellTest :: MonadWriter (Sum Int) m => (m () -> t) -> Int -> t
tellTest f n = f (forM_ [1..n] $ tell . Sum)

modifyTest :: MonadState Int m => (m () -> Int -> t) -> Int -> t
modifyTest f n = f (forM_ [1..n] $ \i -> modify (+ i)) 0

main = defaultMain [
   bgroup "State" [
     bench "CPS"  $ nfIO $ modifyTest CPS.runStateT 100000
     , bench "Strict"$ nfIO $ modifyTest Strict.runStateT 100000
     , bench "Lazy"  $ nfIO $ modifyTest Lazy.runStateT 100000
     ]
  , bgroup "Writer" [
     bench "CPS"  $ nfIO $ tellTest CPS.runWriterT 10000
     , bench "Strict" $ nfIO $ tellTest Strict.runWriterT 10000
     , bench "Lazy" $ nfIO $ tellTest Lazy.runWriterT 10000
     ]
  , bgroup "Reader" [
     bench "CPS" $ nfIO $ askTest CPS.runReaderT 10000
     , bench "Normal" $ nfIO $ askTest Normal.runReaderT 10000
     ]
  ]

