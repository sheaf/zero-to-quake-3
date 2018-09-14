{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}

module Quake3.BSP ( BSP(..), Face(..), MeshVertList(..), VertexList(..), loadBSP ) where

-- base
import Control.Applicative ( liftA2, liftA3 )
import Control.Monad ( guard, replicateM )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Char ( ord )
import Data.Foldable ( traverse_ )
import Data.Int ( Int32 )

-- binary
import qualified Data.Binary.Get

-- bytestring
import qualified Data.ByteString.Lazy


data DirEntry = DirEntry
  { deOffset :: Int32
  , deLength :: Int32
  }
  deriving ( Show )


data BSP = BSP
  { bspVertices  :: VertexList
  , bspMeshVerts :: MeshVertList
  , bspFaces     :: [ Face ]
  }
  deriving ( Show )


newtype VertexList = VertexList
  { vertexListBytes :: Data.ByteString.Lazy.ByteString
  }
  deriving ( Show )


newtype MeshVertList = MeshVertList
  { meshVertListBytes :: Data.ByteString.Lazy.ByteString
  }
  deriving ( Show )


getDirEntry :: Data.Binary.Get.Get DirEntry
getDirEntry =
  DirEntry <$> Data.Binary.Get.getInt32le <*> Data.Binary.Get.getInt32le


loadBSP :: MonadIO m => FilePath -> m BSP
loadBSP bspFilePath = do
  fmap
    ( Data.Binary.Get.runGet getBSP . Data.ByteString.Lazy.fromStrict . Data.ByteString.Lazy.toStrict )
    ( liftIO ( Data.ByteString.Lazy.readFile bspFilePath ) )


getBSP :: Data.Binary.Get.Get BSP
getBSP = do
  fileBytes <-
    Data.Binary.Get.lookAhead Data.Binary.Get.getRemainingLazyByteString

  [ _entities
    , _textures
    , _planes
    , _nodes
    , _leafs
    , _leafFaces
    , _leafBrushes
    , _models
    , _brushes
    , _brushSides
    , vertexes
    , meshVerts
    , _effects
    , faces
    , _lightmaps
    , _lightvols
    , _visdata
    ] <- do
    traverse_
      ( \c -> Data.Binary.Get.getWord8 >>= guard . ( fromIntegral c == ) )
      ( map ord "IBSP" )

    0x2e <-
      Data.Binary.Get.getInt32le

    replicateM 17 getDirEntry

  return
    BSP
      { bspVertices =
          VertexList ( lookupBytes vertexes fileBytes )
      , bspMeshVerts =
          MeshVertList ( lookupBytes meshVerts fileBytes )
      , bspFaces =
          let
            n =
              deLength faces `div` ( 26 * 4 )

          in
          Data.Binary.Get.runGet
            ( replicateM ( fromIntegral n ) getFace )
            ( lookupBytes faces fileBytes )
      }


lookupBytes
  :: DirEntry
  -> Data.ByteString.Lazy.ByteString
  -> Data.ByteString.Lazy.ByteString
lookupBytes DirEntry { deOffset, deLength } bytes =
  Data.ByteString.Lazy.take
    ( fromIntegral deLength )
    ( Data.ByteString.Lazy.drop ( fromIntegral deOffset ) bytes )


data Face = Face
  { faceTexture    ::   Int32
  , faceEffect     ::   Int32
  , faceType       ::   Int32
  , faceVertex     ::   Int32
  , faceNVertexes  ::   Int32
  , faceMeshVert   ::   Int32
  , faceNMeshVerts ::   Int32
  , faceLMIndex    ::   Int32
  , faceLMStart    :: ( Int32, Int32 )
  , faceLMSize     :: ( Int32, Int32 )
  , faceLMOrigin   :: ( Float, Float, Float )
  , faceLMS        :: ( Float, Float, Float )
  , faceLMT        :: ( Float, Float, Float )
  , faceNormal     :: ( Float, Float, Float )
  , faceSize       :: ( Int32, Int32 )
  }
  deriving ( Show )


getFace :: Data.Binary.Get.Get Face
getFace = do
  faceTexture    <- Data.Binary.Get.getInt32le
  faceEffect     <- Data.Binary.Get.getInt32le
  faceType       <- Data.Binary.Get.getInt32le
  faceVertex     <- Data.Binary.Get.getInt32le
  faceNVertexes  <- Data.Binary.Get.getInt32le
  faceMeshVert   <- Data.Binary.Get.getInt32le
  faceNMeshVerts <- Data.Binary.Get.getInt32le
  faceLMIndex    <- Data.Binary.Get.getInt32le
  faceLMStart    <- liftA2 (,)  Data.Binary.Get.getInt32le Data.Binary.Get.getInt32le
  faceLMSize     <- liftA2 (,)  Data.Binary.Get.getInt32le Data.Binary.Get.getInt32le
  faceLMOrigin   <- liftA3 (,,) Data.Binary.Get.getFloatle Data.Binary.Get.getFloatle Data.Binary.Get.getFloatle
  faceLMS        <- liftA3 (,,) Data.Binary.Get.getFloatle Data.Binary.Get.getFloatle Data.Binary.Get.getFloatle
  faceLMT        <- liftA3 (,,) Data.Binary.Get.getFloatle Data.Binary.Get.getFloatle Data.Binary.Get.getFloatle
  faceNormal     <- liftA3 (,,) Data.Binary.Get.getFloatle Data.Binary.Get.getFloatle Data.Binary.Get.getFloatle
  faceSize       <- liftA2 (,)  Data.Binary.Get.getInt32le Data.Binary.Get.getInt32le
  return Face{..}
