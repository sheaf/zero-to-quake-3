{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Quake3.Patch where

-- base
import Control.Arrow( (***) )
import Data.Foldable ( traverse_ )
import Data.Int ( Int32 )

-- binary
import qualified Data.Binary.Put

-- bytestring
import qualified Data.ByteString.Lazy

-- zero-to-quake-3
import Quake3.BSP ( Face(..), MeshVertList(..) )

{-

patch:

 0   1   2   3   4 
 5   6   7   8   9
10  11  12  13  14 
15  16  17  18  19
20  21  22  23  24 

béziers:

 0   1   2      2   3   4 
 5   6   7      7   8   9
10  11  12     12  13  14 


10  11  12     12  13  14
15  16  17     17  18  19
20  21  22     22  23  24

indices:

0 1 2 5 6 7 10 11 12    2 3 4 7 8 9 12 13 14   ...

-}

-- rows, cols denote the number of béziers per row/column,
-- not number of vertices per row/column
patchIndices :: (Int32, Int32) -> Int32 -> ( Data.ByteString.Lazy.ByteString, Int32 )
patchIndices (rows, cols) firstIndex
  = ( Data.Binary.Put.runPut ( traverse_ Data.Binary.Put.putInt32le inds )
    , (2*rows+1)*(2*cols+1)
    )
    where 
      inds 
        = [ firstIndex + (i + 2*c) + (2*cols + 1)*(j + 2*r)
          | r <- [0..rows-1], c <- [0..cols-1], j <- [0..2], i <- [0..2]
          ]

indices :: [ Face ] -> ( MeshVertList, Int32 )
indices faces
  = 
    ( MeshVertList . mconcat *** sum )
    . unzip
    . map 
      ( \face -> patchIndices ( faceSize face ) ( faceVertex face ) )
    . filter ( (== 2) . faceType)
    $ faces
