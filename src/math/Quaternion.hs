{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Math.Quaternion where

import Math.Linear(V(..), M, pattern V3, pattern V4, Vector(..), Inner(..), norm, normalise)

newtype Quaternion a = Quaternion { quaternion :: V 4 a }

deriving instance Show a => Show (Quaternion a)
deriving instance Eq a => Eq (Quaternion a)
deriving instance Functor Quaternion
deriving instance Applicative Quaternion
deriving instance Foldable Quaternion
deriving instance Traversable Quaternion
deriving instance (Num a, RealFloat a) => Vector (Quaternion a)
deriving instance (Num a, RealFloat a) => Inner (Quaternion a)

instance (Num a, RealFloat a) => Num (Quaternion a) where
  (+) = (^+^)
  (-) = (^-^)
  Quaternion { quaternion = V4 xr xi xj xk } * Quaternion { quaternion = V4 yr yi yj yk }
    =  Quaternion $
        V4 (xr*yr - xi*yi - xj*yj - xk*yk)
           (xr*yi + xi*yr + xj*yk - xk*yj)
           (xr*yj + xj*yr + xk*yi - xi*yk)
           (xr*yk + xk*yr + xi*yj - xj*yi)
  abs q    = norm q *^ Quaternion (V4 1 0 0 0)
  signum q = recip (norm q) *^ q
  fromInteger = Quaternion . (:. V3 0 0 0) . fromInteger

conjugate :: (Num a, RealFloat a) => Quaternion a -> Quaternion a
conjugate (Quaternion (a :. v)) = Quaternion (a :. ((-1) *^ v))

-- 'rotate' expects the quaternion to be of unit norm
rotate :: (Num a, RealFloat a) => Quaternion a -> V 3 a -> V 3 a
rotate q v = ijk where
  Quaternion (_ :. ijk) = q * Quaternion (0 :. v) * conjugate q

axisAngle :: (Num a, RealFloat a) => V 3 a -> a -> Quaternion a
axisAngle axis theta = Quaternion ( cos half :. sin half *^ normalise axis )
  where half = 0.5 * theta

fromQuaternion :: (Num a, RealFloat a) => Quaternion a -> M 3 3 a
fromQuaternion (Quaternion (V4 w x y z))
 = V3 (V3 (1-2*(y2+z2)) (2*(xy-zw)) (2*(xz+yw)))
      (V3 (2*(xy+zw)) (1-2*(x2+z2)) (2*(yz-xw)))
      (V3 (2*(xz-yw)) (2*(yz+xw)) (1-2*(x2+y2)))
  where x2 = x*x
        y2 = y*y
        z2 = z*z
        xy = x*y
        xz = x*z
        xw = x*w
        yz = y*z
        yw = y*w
        zw = z*w