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
import Data.Foldable ( traverse_ )
import Data.Maybe ( mapMaybe )

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
import qualified Quake3.BSP
import qualified Quake3.Entity
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

  runManaged
    $ withQuake3Context
    $ \context@Context{..} -> do

        resources <-
          Quake3.Render.initResources context

        commandBuffers <-
          traverse
            ( Quake3.Render.renderToFrameBuffer context resources )
            framebuffers

        let 
          initialEntities =
            mapMaybe
              Quake3.Entity.parseEntity
              ( Quake3.BSP.bspEntities ( Quake3.Render.bsp resources ) )
          initialQ3State = Quake3.Model.initial initialEntities

        q3StateTVar <- liftIO ( STM.newTVarIO ( Just initialQ3State ) )



        liftIO $ concurrently_
          (    ( runExceptT . D.reactimate )
                 ( renderSFReader refTime context resources commandBuffers q3StateTVar )
          )
          (    (runExceptT . D.reactimate )
                  ( gameSFWriter refTime initialQ3State q3StateTVar )
            >> STM.atomically ( STM.writeTVar q3StateTVar Nothing )
          )
  
  putStrLn "goodbye!"



renderSFReader :: MonadIO m 
             => Double
             -> Context
             -> Quake3.Render.Resources
             -> [ Vulkan.VkCommandBuffer ]
             -> STM.TVar (Maybe Quake3State)
             -> D.MSF (ExceptT () m) () ()
renderSFReader refTime context resources commandBuffers tvar = proc () -> do
  dt <- D.liftMSFTrans (dtime refTime) -< ()
  D.runReaderS sf -< (dt, ())
  where sf = ( readerSF tvar
               >>> untilNothing ( renderSF context resources commandBuffers ) 
             ) `every` 0.001 
               >>> hold () -- cap framerate at 1000

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
           -> Quake3State
           -> STM.TVar (Maybe Quake3State)
           -> D.MSF (ExceptT () m) () ()
gameSFWriter refTime initialQ3State tvar = proc () -> do
  dt <- D.liftMSFTrans (dtime refTime) -< ()
  D.runReaderS sf -< (dt, ())
    where sf =
            (    actionSF 
              >>> D.liftMSFTrans ( simulationSF initialQ3State )
              >>> D.arr Just 
              >>> writerSF tvar
            ) `every` fromRational simulationTickTime
              >>> hold ()

untilNothing :: Monad m => D.MSF m a b -> D.MSF (ExceptT () m) (Maybe a) b
untilNothing sf = D.maybeToExceptS ( D.inMaybeT >>> D.liftMSFTrans sf )
