{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Vulkan.Shader where

-- base
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Foreign

-- bytestring
import qualified Data.ByteString

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Vulkan ( managedVulkanResource )

data ShaderType = Vertex 
                | TessellationControl
                | TessellationEvaluation
                | Geometry
                | Fragment
                | Compute
  deriving (Eq, Show, Ord, Enum, Bounded)

shaderBitmask :: ShaderType -> Vulkan.VkShaderStageBitmask a
shaderBitmask Vertex                 = Vulkan.VK_SHADER_STAGE_VERTEX_BIT
shaderBitmask TessellationControl    = Vulkan.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
shaderBitmask TessellationEvaluation = Vulkan.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
shaderBitmask Geometry               = Vulkan.VK_SHADER_STAGE_GEOMETRY_BIT
shaderBitmask Fragment               = Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
shaderBitmask Compute                = Vulkan.VK_SHADER_STAGE_COMPUTE_BIT

data ShaderInfo = ShaderInfo 
  { shaderPath :: FilePath
  , shaderType :: ShaderType
  , entryPoint :: String
  } deriving (Eq, Show)

loadShader :: MonadManaged m => Vulkan.VkDevice -> FilePath -> m Vulkan.VkShaderModule
loadShader device path = do
  bytes <-
    liftIO ( Data.ByteString.readFile path )

  managedVulkanResource
    ( \a b ->
        Data.ByteString.useAsCStringLen bytes $ \( bytesPtr, len ) ->
          let
            createInfo =
              Vulkan.createVk
                (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
                &* Vulkan.set @"pNext" Vulkan.VK_NULL
                &* Vulkan.set @"flags" 0
                &* Vulkan.set @"pCode" ( Foreign.castPtr bytesPtr )
                &* Vulkan.set @"codeSize" ( fromIntegral len )
                )

          in
          Vulkan.vkCreateShaderModule device ( Vulkan.unsafePtr createInfo ) a b
    )
    ( Vulkan.vkDestroyShaderModule device )

createShaderStage :: MonadManaged m => Vulkan.VkDevice -> ShaderInfo -> m Vulkan.VkPipelineShaderStageCreateInfo
createShaderStage device ShaderInfo{..} = do
  shader <- loadShader device shaderPath
  pure $ Vulkan.createVk
    (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
    &* Vulkan.set @"pNext" Vulkan.VK_NULL
    &* Vulkan.set @"flags" 0
    &* Vulkan.setStrRef @"pName" entryPoint
    &* Vulkan.set @"module" shader
    &* Vulkan.set @"stage" (shaderBitmask shaderType)
    )
