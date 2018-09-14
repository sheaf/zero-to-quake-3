{-# language Arrows          #-}
{-# language PatternSynonyms #-}
{-# language RecordWildCards #-}


module Main ( main ) where

-- base
import Control.Arrow
import Control.Monad.IO.Class ( MonadIO, liftIO )

-- dunai
import qualified Data.MonadicStreamFunction as D

-- managed
import Control.Monad.Managed ( runManaged )

-- transformers
import Control.Monad.Trans.Except( ExceptT, runExceptT )

-- vulkan-api
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
  ( vkQueueWaitIdle
  , VkCommandBuffer
  )

-- zero-to-quake-3
import Foreign.Vulkan ( throwVkResult )
import Quake3.Context ( Context(..), withQuake3Context )
import Quake3.Input ( actionSF )
import qualified Quake3.Render
import Quake3.Model ( Quake3State, simulationSF )
import Vulkan.CommandBuffer ( submitCommandBuffer )
import Vulkan.WSI ( acquireNextImage , present )

main :: IO ()
main =
  runManaged
    $ withQuake3Context
    $ \context@Context{..} -> do
        resources <-
          Quake3.Render.initResources context

        commandBuffers <-
          traverse
            ( Quake3.Render.renderToFrameBuffer context resources )
            framebuffers

        _ <- runExceptT 
              ( D.reactimate 
                  ( q3SF context resources commandBuffers )
              )

        pure ()

q3SF :: MonadIO m
     => Context 
     -> Quake3.Render.Resources
     -> [ Vulkan.VkCommandBuffer ] 
     -> D.MSF (ExceptT () m) () ()
q3SF context resources commandBuffers
   = actionSF
   >>> safely ( simulationSF 
                >>> renderSF context resources commandBuffers
              )
      where safely :: Monad n => D.MSF n a b -> D.MSF (ExceptT e n) a b
            safely = D.liftMSFTrans

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
      
      _ <- D.arrM ( Quake3.Render.updateUniformBufferFromModel resources ) -< q3State

      _ <- D.arrM ( \buff -> submitCommandBuffer queue buff nextImageSem submitted ) -< commandBuffer

      _ <- D.arrM ( \nextIm -> present queue swapchain nextIm submitted ) -< nextImageIndex

      _ <- D.arrM_ ( liftIO ( Vulkan.vkQueueWaitIdle queue ) 
                    >>= throwVkResult
                 ) -< ()

      returnA -< ()
