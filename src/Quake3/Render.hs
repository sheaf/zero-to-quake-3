{-# language DataKinds          #-}
{-# language GADTs              #-}
{-# language PatternSynonyms    #-}
{-# language RankNTypes         #-}
{-# language RecordWildCards    #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications   #-}

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
import qualified Foreign.C

-- contravariant
import Data.Functor.Contravariant

-- linear
import Math.Linear
  ( (!*!)
  , (^+^)
  , identity
  , lookAt
  , perspective
  , V(..)
  , pattern V2
  , pattern V3
  )
import Math.Coordinates(Representation(..), MatrixWithRep(..), q3ToVk)
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
import qualified Quake3.Patch
import Vulkan.Pipeline ( bindPipeline )
import qualified Vulkan.Poke
import Vulkan.RenderPass ( withRenderPass )


data Resources = Resources
  { vertexBuffer  :: VertexBuffer VertexList
  , indexBuffer   :: IndexBuffer MeshVertList
  , uniformBuffer :: UniformBuffer ( MatrixWithRep 'RowMajor 4 4 Foreign.C.CFloat )
  , q3map         :: BSP
  }


initResources :: MonadManaged m => Context -> m Resources
initResources Context{..} = do
  q3map <-
    loadBSP "BSP/q3dm1.bsp"

  vertexBuffer <-
    createVertexBuffer
      physicalDevice
      device
      ( contramap vertexListBytes Vulkan.Poke.pokeLazyBytestring )
      ( bspVertices q3map )

  let (indices, _) = Quake3.Patch.indices ( bspFaces q3map )

  indexBuffer <-
    createIndexBuffer
      physicalDevice
      device
      ( contramap meshVertListBytes Vulkan.Poke.pokeLazyBytestring )
      ( indices ) --( bspMeshVerts q3map )

  uniformBuffer <-
    createUniformBuffer
      physicalDevice
      device
      Vulkan.Poke.storable
      ( modelViewProjection (V3 0 0 0) (V2 0 0) )

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

      -- TODO: shouldn't be computing this here
      let (_, nbIndices) = Quake3.Patch.indices ( bspFaces q3map )

      liftIO $
        Vulkan.vkCmdDrawIndexed
          commandBuffer
          (fromIntegral nbIndices) -- indexCount
          1                        -- instanceCount
          0                        -- firstIndex
          0                        -- vertexOffset
          0                        -- firstInstance

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
  -> MatrixWithRep 'RowMajor 4 4 Foreign.C.CFloat
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

    model = identity

    projection =
      perspective ( pi / 2 ) ( 4 / 3 ) 0.1 100000

  in MatrixWithRep (projection !*! view !*! model !*! q3ToVk)
