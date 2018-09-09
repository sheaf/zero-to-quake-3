{-# LANGUAGE GADTs           #-}

module Vulkan.PushConstants
  ( PushConstants(..)
  , updatePushConstants
  ) where

import Data.Coerce(coerce)

import qualified Foreign

import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

data PushConstants where
  AnyConstants :: Foreign.Storable a => a -> Vulkan.Word32 -> PushConstants

updatePushConstants 
  :: Vulkan.VkCommandBuffer 
  -> Vulkan.VkPipelineLayout
  -> PushConstants
  -> IO ()
updatePushConstants commandBuffer pipelineLayout (AnyConstants constants size)
  = Foreign.with constants $ \ptr ->
      Vulkan.vkCmdPushConstants
        commandBuffer
        pipelineLayout
        Vulkan.VK_SHADER_STAGE_ALL
        0 -- offset
        size
        (coerce ptr)