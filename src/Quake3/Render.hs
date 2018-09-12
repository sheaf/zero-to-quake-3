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
import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Coerce ( coerce )
import Data.Foldable ( for_ )
import Data.Word ( Word32 )
import qualified Foreign.C
import qualified Data.ByteString.Lazy

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
import Quake3.BSP
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
  , q3map :: BSP
  }


initResources :: MonadManaged m => Context -> m Resources
initResources Context{..} = do
  q3map <-
    loadBSP "BSP/q3dm1.bsp"

  vertexBuffer <-
    createVertexBuffer physicalDevice device ( bspVertices q3map )

  indexBuffer <-
    createIndexBuffer physicalDevice device ( bspMeshVerts q3map )

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
        ( for_ ( bspFaces q3map ) $ \face ->
            when ( faceType face == 1 ) $
              Vulkan.vkCmdDrawIndexed
                commandBuffer
                ( fromIntegral ( faceNMeshVerts face ) ) -- indexCount
                1                                        -- instanceCount
                ( fromIntegral ( faceMeshVert face ) )   -- firstIndex
                ( fromIntegral ( faceVertex face ) )     -- vertexOffset
                0                                        -- firstInstance
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
        rotate = identity
        {-
            Quaternion.fromQuaternion
                ( Quaternion.axisAngle ( V3 1 1 1 )  ( pi / 5 ) )
            `blockSum` identity
        -}

        translate = translation (V3 0 0 (-5))

      in
      translate !*! rotate

    projection =
      perspective ( pi / 2 ) ( 4 / 3 ) 0.1 100000

  in transpose ( projection !*! view !*! model )