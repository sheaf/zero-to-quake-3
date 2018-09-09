{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Vulkan.PushConstants
  ( PushConstants(..)
  , updatePushConstants
  ) where

import Data.Coerce(coerce)

import qualified Foreign

import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan

data PushConstants where
  AnyConstants :: (Show a, Foreign.Storable a) => a -> PushConstants

deriving instance Show PushConstants

updatePushConstants 
  :: Vulkan.VkCommandBuffer 
  -> Vulkan.VkPipelineLayout
  -> Vulkan.Word32
  -> PushConstants
  -> IO ()
updatePushConstants commandBuffer pipelineLayout size (AnyConstants constants)
  = Foreign.with constants $ \ptr ->
      Vulkan.vkCmdPushConstants
        commandBuffer
        pipelineLayout
        Vulkan.VK_SHADER_STAGE_ALL
        0 -- offset
        size
        (coerce ptr)