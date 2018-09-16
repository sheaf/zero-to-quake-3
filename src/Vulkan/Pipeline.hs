{-# language DataKinds #-}
{-# language TypeApplications #-}

module Vulkan.Pipeline
  ( createPipeline
  , bindPipeline
  ) where

-- base
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Bits

-- managed
import Control.Monad.Managed ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- zero-to-quake-3
import Foreign.Vulkan ( managedVulkanResource )
import qualified Vulkan.VertexFormat as VertexFormat
import Vulkan.Shader( ShaderInfo(..), ShaderType(..), createShaderStage )


createPipeline
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkExtent2D
  -> Vulkan.VkDescriptorSetLayout
  -> VertexFormat.VertexFormat v
  -> [ ShaderInfo ]
  -> m ( Vulkan.VkPipeline, Vulkan.VkPipelineLayout )
createPipeline device renderPass extent layout0 vertexFormat shaderInfos = do
  pipelineLayout <-
    let
      pipelineLayoutCreateInfo =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"flags" 0
          &* Vulkan.set @"setLayoutCount" 1
          &* Vulkan.setListRef @"pSetLayouts" [ layout0 ]
          &* Vulkan.set @"pushConstantRangeCount" 0
          &* Vulkan.setListRef @"pPushConstantRanges" [ ]
          )

    in
    managedVulkanResource
      ( Vulkan.vkCreatePipelineLayout
          device
          ( Vulkan.unsafePtr pipelineLayoutCreateInfo )
      )
      ( Vulkan.vkDestroyPipelineLayout device )

  shaderStages <- 
      traverse (createShaderStage device)
               shaderInfos

  let

    needsTessellation = any ( (==TessellationEvaluation) . shaderType ) shaderInfos
    topology = if needsTessellation
               then Vulkan.VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
               else Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST

    tessellationStateCreateInfo = 
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"patchControlPoints" 9
        )

    rasterizationCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"depthClampEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"rasterizerDiscardEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"polygonMode" Vulkan.VK_POLYGON_MODE_FILL
        &* Vulkan.set @"lineWidth" 1
        &* Vulkan.set @"depthBiasEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"depthBiasSlopeFactor" 0
        &* Vulkan.set @"depthBiasClamp" 0
        &* Vulkan.set @"depthBiasConstantFactor" 0
        &* Vulkan.set @"frontFace" Vulkan.VK_FRONT_FACE_CLOCKWISE
        &* Vulkan.set @"cullMode" Vulkan.VK_CULL_MODE_BACK_BIT
        )

    vertexBindingDescription =
      Vulkan.createVk
        (  Vulkan.set @"binding" 0
        &* Vulkan.set @"stride" ( fromIntegral ( VertexFormat.strideSize vertexFormat ) )
        &* Vulkan.set @"inputRate" Vulkan.VK_VERTEX_INPUT_RATE_VERTEX
        )

    vertexInputAttributeDescriptions =
      VertexFormat.attributeDescriptions 0 vertexFormat

    vertexInputState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"vertexBindingDescriptionCount" 1
        &* Vulkan.setListRef @"pVertexBindingDescriptions" [ vertexBindingDescription ]
        &* Vulkan.set @"vertexAttributeDescriptionCount" ( fromIntegral ( length vertexInputAttributeDescriptions ) )
        &* Vulkan.setListRef @"pVertexAttributeDescriptions" vertexInputAttributeDescriptions
        )

    assemblyStateCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"topology" topology
        &* Vulkan.set @"primitiveRestartEnable" Vulkan.VK_FALSE
        )

    viewport =
      Vulkan.createVk
        (  Vulkan.set @"x" 0
        &* Vulkan.set @"y" 0
        &* Vulkan.set @"width" ( fromIntegral ( Vulkan.getField @"width" extent ) )
        &* Vulkan.set @"height" ( fromIntegral ( Vulkan.getField @"height" extent ) )
        &* Vulkan.set @"minDepth" 0
        &* Vulkan.set @"maxDepth" 1
        )

    scissor =
      let
        offset =
          Vulkan.createVk
            (  Vulkan.set @"x" 0
            &* Vulkan.set @"y" 0
            )

      in
      Vulkan.createVk
        (  Vulkan.set @"offset" offset
        &* Vulkan.set @"extent" extent
        )

    viewportState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"viewportCount" 1
        &* Vulkan.set @"scissorCount" 1
        &* Vulkan.setListRef @"pViewports" [ viewport ]
        &* Vulkan.setListRef @"pScissors" [ scissor ]
        )

    multisampleState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
        &* Vulkan.set @"minSampleShading" 1
        &* Vulkan.set @"rasterizationSamples" Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        )

    attachmentState =
      Vulkan.createVk
        ( Vulkan.set @"blendEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"alphaBlendOp" Vulkan.VK_BLEND_OP_ADD
        &* Vulkan.set @"srcColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
        &* Vulkan.set @"dstColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
        &* Vulkan.set @"colorBlendOp" Vulkan.VK_BLEND_OP_ADD
        &* Vulkan.set @"srcAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
        &* Vulkan.set @"dstAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
        &* Vulkan.set
             @"colorWriteMask"
             ( Vulkan.VK_COLOR_COMPONENT_R_BIT
                 .|. Vulkan.VK_COLOR_COMPONENT_G_BIT
                 .|. Vulkan.VK_COLOR_COMPONENT_B_BIT
                 .|. Vulkan.VK_COLOR_COMPONENT_A_BIT
             )
        )

    colorBlendState =
      Vulkan.createVk
        ( Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
        &* Vulkan.setAt @"blendConstants" @0 0
        &* Vulkan.setAt @"blendConstants" @1 0
        &* Vulkan.setAt @"blendConstants" @2 0
        &* Vulkan.setAt @"blendConstants" @3 0
        &* Vulkan.set @"attachmentCount" 1
        &* Vulkan.setListRef @"pAttachments" [ attachmentState ]
        &* Vulkan.set @"logicOp" Vulkan.VK_LOGIC_OP_COPY
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        )

    nullStencilOp =
      Vulkan.createVk
        (  Vulkan.set @"reference" 0
        &* Vulkan.set @"writeMask" 0
        &* Vulkan.set @"compareMask" 0
        &* Vulkan.set @"compareOp" Vulkan.VK_COMPARE_OP_EQUAL
        &* Vulkan.set @"depthFailOp" Vulkan.VK_STENCIL_OP_KEEP
        &* Vulkan.set @"passOp" Vulkan.VK_STENCIL_OP_KEEP
        &* Vulkan.set @"failOp" Vulkan.VK_STENCIL_OP_KEEP
        )

    depthStencilState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" 0
        &* Vulkan.set @"depthTestEnable" Vulkan.VK_TRUE
        &* Vulkan.set @"depthWriteEnable" Vulkan.VK_TRUE
        &* Vulkan.set @"depthCompareOp" Vulkan.VK_COMPARE_OP_LESS_OR_EQUAL
        &* Vulkan.set @"maxDepthBounds" 1
        &* Vulkan.set @"minDepthBounds" 0
        &* Vulkan.set @"stencilTestEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"front" nullStencilOp
        &* Vulkan.set @"back" nullStencilOp
        )

    createInfo =
      if needsTessellation
      then 
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"flags" 0
          &* Vulkan.set @"stageCount" ( fromIntegral ( length shaderStages ) )
          &* Vulkan.setListRef @"pStages" shaderStages
          &* Vulkan.setVkRef @"pVertexInputState" vertexInputState
          &* Vulkan.setVkRef @"pInputAssemblyState" assemblyStateCreateInfo
          &* Vulkan.setVkRef @"pTessellationState" tessellationStateCreateInfo
          &* Vulkan.set @"basePipelineIndex" 0
          &* Vulkan.set @"subpass" 0
          &* Vulkan.set @"renderPass" renderPass
          &* Vulkan.set @"layout" pipelineLayout
          &* Vulkan.setVkRef @"pRasterizationState" rasterizationCreateInfo
          &* Vulkan.setVkRef @"pViewportState" viewportState
          &* Vulkan.setVkRef @"pMultisampleState" multisampleState
          &* Vulkan.setVkRef @"pColorBlendState" colorBlendState
          &* Vulkan.setVkRef @"pDepthStencilState" depthStencilState
          )
      else
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"flags" 0
          &* Vulkan.set @"stageCount" ( fromIntegral ( length shaderStages ) )
          &* Vulkan.setListRef @"pStages" shaderStages
          &* Vulkan.setVkRef @"pVertexInputState" vertexInputState
          &* Vulkan.setVkRef @"pInputAssemblyState" assemblyStateCreateInfo
          &* Vulkan.set @"basePipelineIndex" 0
          &* Vulkan.set @"subpass" 0
          &* Vulkan.set @"renderPass" renderPass
          &* Vulkan.set @"layout" pipelineLayout
          &* Vulkan.setVkRef @"pRasterizationState" rasterizationCreateInfo
          &* Vulkan.setVkRef @"pViewportState" viewportState
          &* Vulkan.setVkRef @"pMultisampleState" multisampleState
          &* Vulkan.setVkRef @"pColorBlendState" colorBlendState
          &* Vulkan.setVkRef @"pDepthStencilState" depthStencilState
          )


  pipeline <-
    managedVulkanResource
      ( Vulkan.vkCreateGraphicsPipelines
          device
          Vulkan.vkNullPtr
          1
          ( Vulkan.unsafePtr createInfo )
      )
      ( Vulkan.vkDestroyPipeline device )

  return ( pipeline, pipelineLayout )


bindPipeline :: MonadIO m => Vulkan.VkCommandBuffer -> Vulkan.VkPipeline -> m ()
bindPipeline commandBuffer graphicsPipeline =
  liftIO
    ( Vulkan.vkCmdBindPipeline
        commandBuffer
        Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
        graphicsPipeline
    )
