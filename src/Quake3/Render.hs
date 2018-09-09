{-# language DataKinds          #-}
{-# language GADTs              #-}
{-# language PatternSynonyms    #-}
{-# language RankNTypes         #-}
{-# language RecordWildCards    #-}
{-# language StandaloneDeriving #-}

module Quake3.Render
  ( Resources
  , initResources
  , renderToFrameBuffer
  , updateUniformBufferFromModel
  ) where

-- base
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Coerce ( coerce )
import Data.Word ( Word32 )
import qualified Foreign.C

-- linear
import Math.Linear
  ( (!*!)
  , (^+^)
  , identity
  , blockSum
  , lookAt
  , perspective
  , translation
  , transpose
  , V(..)
  , M
  , pattern V2
  , pattern V3
  )
import qualified Math.Quaternion as Quaternion

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
  ( vkCmdDrawIndexed
  , VkCommandBuffer
  , VkFramebuffer
  )

-- zero-to-quake-3
import Quake3.Context ( Context(..) )
import qualified Quake3.Model
import Vulkan.Buffer ( pokeBuffer )
import Vulkan.Buffer.IndexBuffer
  ( IndexBuffer
  , bindIndexBuffer
  , createIndexBuffer
  )
import Vulkan.Buffer.UniformBuffer ( UniformBuffer(..), createUniformBuffer )
import Vulkan.Buffer.VertexBuffer
  ( VertexBuffer
  , bindVertexBuffers
  , createVertexBuffer
  )
import Vulkan.CommandBuffer
  ( allocateCommandBuffer
  , withCommandBuffer
  )
import Vulkan.DescriptorSet
  ( bindDescriptorSets
  , updateDescriptorSet
  )
import Vulkan.Pipeline ( bindPipeline )
import Vulkan.RenderPass ( withRenderPass )


data Resources = Resources
  { vertexBuffer  :: VertexBuffer
  , indexBuffer   :: IndexBuffer
  , uniformBuffer :: UniformBuffer
  , indices       :: [ Word32 ]
  }


initResources :: MonadManaged m => Context -> m Resources
initResources Context{..} = do
  let
    vertices =
      [ V2 ( V3 (-1)   1    1  ) ( V3 0 0 0 )
      , V2 ( V3   1    1    1  ) ( V3 0 0 0 )
      , V2 ( V3 (-1) (-1)   1  ) ( V3 0 0 0 )
      , V2 ( V3   1  (-1)   1  ) ( V3 0 0 0 )
      , V2 ( V3 (-1)   1  (-1) ) ( V3 1 0 0 )
      , V2 ( V3   1    1  (-1) ) ( V3 1 1 0 )
      , V2 ( V3 (-1) (-1) (-1) ) ( V3 0 1 0 )
      , V2 ( V3   1  (-1) (-1) ) ( V3 0 1 1 )
      ]

    indices =
      [ 2, 3, 0, 3, 0, 1
      , 6, 7, 4, 7, 4, 5
      ]

  vertexBuffer <-
    createVertexBuffer physicalDevice device vertices

  indexBuffer <-
    createIndexBuffer physicalDevice device indices

  uniformBuffer <-
    createUniformBuffer physicalDevice device ( modelViewProjection (V3 0 0 0) (V2 0 0) )

  updateDescriptorSet device descriptorSet uniformBuffer
  
  return Resources{..}


renderToFrameBuffer
  :: MonadManaged m
  => Context -> Resources -> Vulkan.VkFramebuffer -> m Vulkan.VkCommandBuffer
renderToFrameBuffer Context{..} Resources{..} framebuffer = do
  commandBuffer <-
    allocateCommandBuffer device commandPool

  withCommandBuffer commandBuffer $ do

    bindVertexBuffers commandBuffer [ vertexBuffer ]

    bindIndexBuffer commandBuffer indexBuffer

    withRenderPass commandBuffer renderPass framebuffer extent $ do
      bindPipeline commandBuffer graphicsPipeline

      bindDescriptorSets commandBuffer pipelineLayout [ descriptorSet ]

      liftIO 
        ( Vulkan.vkCmdDrawIndexed
            commandBuffer
            ( fromIntegral ( length indices ) )
            1 -- instanceCount
            0 -- firstIndex
            0 -- vertexOffset
            0 -- firstInstance
        )

  return commandBuffer


updateUniformBufferFromModel
  :: MonadIO m
  => Resources 
  -> Quake3.Model.Quake3State
  -> m ()
updateUniformBufferFromModel Resources{..} Quake3.Model.Quake3State{..} =
  pokeBuffer
    ( coerce uniformBuffer )
    ( modelViewProjection cameraPosition cameraAngles )


modelViewProjection
  :: V 3 Foreign.C.CFloat
  -> V 2 Foreign.C.CFloat
  -> M 4 4 Foreign.C.CFloat
modelViewProjection cameraPosition ( V2 x y ) =
  let
    view =
      let
        orientation =
          Quaternion.axisAngle ( V3 0 1 0 ) x
            * Quaternion.axisAngle ( V3 1 0 0 ) y

        forward =
          Quaternion.rotate orientation ( V3 0 0 (-1) )

        up =
          Quaternion.rotate orientation ( V3 0 1 0 )

      in
      lookAt cameraPosition ( cameraPosition ^+^ forward ) up

    model =
      let
        rotate :: M 4 4 Foreign.C.CFloat
        rotate =
            Quaternion.fromQuaternion
                ( Quaternion.axisAngle ( V3 1 1 1 )  ( pi / 5 ) )
            `blockSum` identity

        translate = translation (V3 0 0 (-5))

      in
      translate !*! rotate

    projection =
      perspective ( pi / 6 ) ( 4 / 3 ) 0.1 100

  in transpose ( projection !*! view !*! model )