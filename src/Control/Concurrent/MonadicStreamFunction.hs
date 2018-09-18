{-# LANGUAGE Arrows              #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.MonadicStreamFunction where

-- base
import Control.Arrow
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Maybe ( fromMaybe )

-- dunai
import qualified Data.MonadicStreamFunction as D
import qualified Control.Monad.Trans.MSF as D

-- sdl2
import qualified SDL

-- stm
import qualified Control.Monad.STM as STM
import qualified Control.Concurrent.STM.TVar as STM

-- transformers
import Control.Monad.Trans.Class( lift )
import Control.Monad.Trans.Reader( ReaderT, ask )
import Control.Monad.Trans.State.Strict( StateT, get, put )


writerSF :: MonadIO m => STM.TVar a -> D.MSF m a ()
writerSF tvar = D.arrM ( liftIO . STM.atomically . STM.writeTVar tvar )

readerSF :: MonadIO m => STM.TVar a -> D.MSF m () a
readerSF tvar = D.arrM_ ( liftIO ( STM.readTVarIO tvar ) )


every :: forall m a b. Monad m => D.MSF m a b -> Double -> D.MSF (ReaderT Double m) a (Maybe b)
every sf frameTime = D.runStateS__ sfWithTotalTime 0
  where sf' :: D.MSF (StateT Double (ReaderT Double m)) a b
        sf' = D.liftMSFTrans (D.liftMSFTrans sf)
        sfWithTotalTime :: D.MSF (StateT Double (ReaderT Double m)) a (Maybe b)
        sfWithTotalTime = proc a -> do
          dt    <- D.arrM_ (lift ask) -< ()
          total <- D.arrM_ get        -< ()
          if total + dt >= frameTime
          then do D.arrM put -< total + dt - frameTime
                  b <- sf'   -< a
                  returnA    -< Just b
          else do D.arrM put -< total + dt
                  returnA    -< Nothing

hold :: Monad m => a -> D.MSF m (Maybe a) a
hold a0 = D.feedback a0 . arr 
        $ \(ma,a) ->
            (\x -> (x,x)) (fromMaybe a ma)

-- time in seconds
time :: MonadIO m => D.MSF m () Double
time = D.arrM (const SDL.time)

-- time difference, in seconds
dtime :: MonadIO m => Double -> D.MSF m () Double
dtime refTime = proc _ -> do
  t2 <- time -< ()
  t1 <- D.next refTime time -< ()
  returnA -< t2-t1