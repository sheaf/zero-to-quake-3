{-# language DataKinds       #-}
{-# language RecordWildCards #-}

module Quake3.Vertex ( Vertex(..) ) where

-- base
import Data.Word ( Word8 )
import qualified Foreign
import qualified Foreign.C

-- linear
import Math.Linear(V(..))

data Vertex = Vertex
  { vPos        :: V 3 Foreign.C.CFloat
  , vSurfaceUV  :: V 2 Foreign.C.CFloat
  , vLightmapUV :: V 2 Foreign.C.CFloat
  , vNormal     :: V 3 Foreign.C.CFloat
  , vColor      :: V 4 Word8
  }


instance Foreign.Storable Vertex where
  sizeOf ~Vertex{..} =
    sum
      [ Foreign.sizeOf vPos
      , Foreign.sizeOf vSurfaceUV
      , Foreign.sizeOf vLightmapUV
      , Foreign.sizeOf vNormal
      , Foreign.sizeOf vColor
      ]
