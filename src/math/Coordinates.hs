
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RoleAnnotations    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}


module Math.Coordinates where

-- base
import GHC.TypeLits(KnownNat, type (+))
import qualified Foreign
import qualified Foreign.C

-- zero-to-quake-3
import Math.Linear(V(..), M, repeatV, zipWithV, transpose, (<++>), (^+^), (^-^), (!*!), inverse, pattern V1, pattern V3)



-- TODO: depth information
data AffineCoords n a = Coords 
  { origin         :: V n a
  , basis          :: V n (V n a) -- positive x, positive y, positive z, ...
  }

deriving instance (Eq   a, KnownNat n) => Eq   (AffineCoords n a)
deriving instance (Show a, KnownNat n) => Show (AffineCoords n a)


data Representation = RowMajor | ColumnMajor
  deriving (Eq, Show)

newtype MatrixWithRep (rep :: Representation) m n a = MatrixWithRep { matrixWithRep :: M m n a }
type role MatrixWithRep nominal nominal nominal nominal

instance (KnownNat n, KnownNat m, Foreign.Storable a) => Foreign.Storable (MatrixWithRep 'ColumnMajor m n a) where
  sizeOf    (MatrixWithRep m) = Foreign.sizeOf    m
  alignment (MatrixWithRep m) = Foreign.alignment m
  poke ptr  (MatrixWithRep m) = Foreign.poke      (Foreign.castPtr ptr) m
  peek ptr 
    = fmap 
        MatrixWithRep
        ( Foreign.peek ( Foreign.castPtr ptr ) )

instance (KnownNat n, KnownNat m, Foreign.Storable a) => Foreign.Storable (MatrixWithRep 'RowMajor m n a) where
  sizeOf    (MatrixWithRep m) = Foreign.sizeOf    m
  alignment (MatrixWithRep m) = Foreign.alignment m
  poke ptr  (MatrixWithRep m) = Foreign.poke      (Foreign.castPtr ptr) (transpose m)
  peek ptr 
    = fmap 
        ( MatrixWithRep . transpose )
         ( Foreign.peek ( Foreign.castPtr ptr ) )



convert :: (KnownNat n, Num a, RealFloat a, n ~ 3) => AffineCoords n a -> AffineCoords n a -> M (n+1) (n+1) a
convert 
  Coords { origin         = originFrom
         , basis          = basisFrom
         }
  Coords { origin         = originTo
         , basis          = basisTo
         } 
  = zipWithV 
      ( \r t -> r <++> V1 t ) 
      ( basisTo !*! inverse basisFrom ) 
      ( originTo ^-^ originFrom )
    <++> 
      V1 (repeatV 0 <++> V1 1)

q3Coords :: AffineCoords 3 Foreign.C.CFloat
q3Coords = Coords 
  { origin = V3 0 0 0
  , basis  = V3 ( V3 1 0   0  )
                ( V3 0 0 (-1) )
                ( V3 0 1   0  )
  }

vkCoords :: AffineCoords 3 Foreign.C.CFloat
vkCoords = Coords
  { origin = V3 0 0 0
  , basis  = V3 ( V3 1   0  0 )
                ( V3 0 (-1) 0 )
                ( V3 0   0  1 )
  }

q3ToVk :: M 4 4 Foreign.C.CFloat
q3ToVk = convert q3Coords vkCoords

