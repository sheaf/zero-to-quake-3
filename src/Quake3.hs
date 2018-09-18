{-# language Arrows              #-}
{-# language PatternSynonyms     #-}
{-# language RecordWildCards     #-}

module Main ( main ) where

-- async
import Control.Concurrent.Async ( concurrently_ )

-- base
import Control.Arrow
import Control.Concurrent ( setNumCapabilities )
import Control.Monad.IO.Class ( MonadIO, liftIO )

-- dunai
import qualified Data.MonadicStreamFunction as D
import qualified Control.Monad.Trans.MSF as D

-- managed
import Control.Monad.Managed ( MonadManaged, runManaged )

-- sdl2
import qualified SDL

-- stm
import qualified Control.Monad.STM as STM
import qualified Control.Concurrent.STM.TVar as STM

-- transformers
import Control.Monad.Trans.Except( ExceptT, runExceptT )

-- vulkan-api
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
  ( vkQueueWaitIdle
  , VkCommandBuffer
  )

-- zero-to-quake-3
import Control.Concurrent.MonadicStreamFunction ( readerSF, writerSF, every, dtime, hold )
import Foreign.Vulkan ( throwVkResult )
import Quake3.Constants ( simulationTickTime )
import Quake3.Context ( Context(..), withQuake3Context )
import Quake3.Input ( actionSF )
import qualified Quake3.Render
import Quake3.Model ( Quake3State, simulationSF, initial )
import Vulkan.CommandBuffer ( submitCommandBuffer )
import Vulkan.WSI ( acquireNextImage , present )

logMsg :: MonadManaged m => String -> m ()
logMsg = liftIO . putStrLn

main :: IO ()
main = do

  refTime <- SDL.time
  setNumCapabilities 2 -- needs --threaded command line option

  let initialQ3State = Just Quake3.Model.initial
  q3StateTVar <- STM.newTVarIO initialQ3State

  runManaged
    $ withQuake3Context
    $ \context@Context{..} -> do

        resources <-
          Quake3.Render.initResources context

        commandBuffers <-
          traverse
            ( Quake3.Render.renderToFrameBuffer context resources )
            framebuffers        

        liftIO $ concurrently_
          (    runExceptT ( D.reactimate ( renderSFReader context resources commandBuffers q3StateTVar ) )
          )
          (    runExceptT ( D.reactimate ( gameSFWriter refTime q3StateTVar ) )
            >> STM.atomically ( STM.writeTVar q3StateTVar Nothing )
          )
  
  putStrLn "goodbye!"



renderSFReader :: MonadIO m 
             => Context
             -> Quake3.Render.Resources
             -> [ Vulkan.VkCommandBuffer ]
             -> STM.TVar (Maybe Quake3State)
             -> D.MSF (ExceptT () m) () ()
renderSFReader context resources commandBuffers tvar
  = readerSF tvar
    >>> untilNothing ( renderSF context resources commandBuffers ) 

renderSF :: MonadIO m
         => Context
         -> Quake3.Render.Resources
         -> [ Vulkan.VkCommandBuffer ]
         -> D.MSF m Quake3State ()
renderSF Context{..} resources commandBuffers
  = proc q3State -> do

      nextImageIndex <- D.arrM_ ( acquireNextImage device swapchain nextImageSem  ) -< ()

      let commandBuffer =
            commandBuffers !! nextImageIndex
      
      D.arrM  ( Quake3.Render.updateUniformBufferFromModel resources )              -< q3State
      D.arrM  ( \buff -> submitCommandBuffer queue buff nextImageSem submitted )    -< commandBuffer
      D.arrM  ( \nextIm -> present queue swapchain nextIm submitted )               -< nextImageIndex
      D.arrM_ ( liftIO ( Vulkan.vkQueueWaitIdle queue ) >>= throwVkResult )         -< ()

      returnA -< ()

gameSFWriter :: MonadIO m
           => Double
           -> STM.TVar (Maybe Quake3State)
           -> D.MSF (ExceptT () m) () ()
gameSFWriter refTime tvar
  = gameSF refTime 
    >>> D.arr Just 
    >>> writerSF tvar

gameSF :: MonadIO m 
       => Double 
       -> D.MSF (ExceptT () m) () Quake3State
gameSF refTime = proc () -> do
  dt <- D.liftMSFTrans (dtime refTime) -< ()
  D.runReaderS sf -< (dt, ())
    where sf = ( actionSF >>> D.liftMSFTrans simulationSF ) `every` fromRational simulationTickTime
               >>> hold ( initial :: Quake3State )

untilNothing :: Monad m => D.MSF m a b -> D.MSF (ExceptT () m) (Maybe a) b
untilNothing sf = D.maybeToExceptS ( D.inMaybeT >>> D.liftMSFTrans sf )