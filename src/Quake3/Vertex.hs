{-# language DataKinds       #-}
{-# language RecordWildCards #-}

module Quake3.Vertex ( vertexFormat ) where

-- base
import Data.Word ( Word8 )
import qualified Foreign.C

-- contravariant
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible ( Divisible, divided )

-- linear
import Math.Linear ( V(..) )

-- zero-to-quake-3
import Vulkan.VertexFormat

data Vertex = Vertex
  { vPos        :: V 3 Foreign.C.CFloat
  , vSurfaceUV  :: V 2 Foreign.C.CFloat
  , vLightmapUV :: V 2 Foreign.C.CFloat
  , vNormal     :: V 3 Foreign.C.CFloat
  , vColor      :: V 4 Word8
  }

vertexFormat :: VertexFormat Vertex
vertexFormat =
  vertex
    >$< v3_32sfloat
    >*< v2_32sfloat
    >*< v2_32sfloat
    >*< v3_32sfloat
    >*< v4_8uint

  where
    vertex Vertex{..} =
      ( vPos, ( vSurfaceUV, ( vLightmapUV, ( vNormal, vColor ) ) ) )


infixr 5 >*<

(>*<) :: Divisible f => f a -> f b -> f (a, b)
(>*<) = divided


