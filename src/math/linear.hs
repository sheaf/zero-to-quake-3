{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Math.Linear where

import Control.Applicative(liftA2)
import Data.Kind(Type)
import Data.Type.Equality((:~:)(..))
import Data.Proxy(Proxy(..))
import Foreign.Ptr(Ptr, castPtr)
import Foreign.Storable(Storable(alignment, sizeOf, peek, poke, peekElemOff, pokeElemOff))
import GHC.Base(Int#, Int(I#), (+#))
import GHC.TypeLits(Nat, KnownNat, natVal, type (+), type (-))
import GHC.TypeLits.Compare
import GHC.TypeNats(type (<=?))
import Unsafe.Coerce(unsafeCoerce)

infixr 3 :.
infix  4 .:

data V :: Nat -> Type -> Type where
  Nil :: V 0 a
  (:.) :: KnownNat n => a -> V n a -> V (n+1) a

(.:) :: a -> a -> V 2 a
(.:) a b = a :. b :. Nil

deriving instance KnownNat n => Functor     (V n)
deriving instance KnownNat n => Foldable    (V n)
deriving instance KnownNat n => Traversable (V n)
deriving instance (KnownNat n, Show a) => Show (V n a)

instance (KnownNat n, Semigroup a) => Semigroup (V n a) where
  (<>) = liftA2 (<>)

instance (KnownNat n, Monoid a) => Monoid (V n a) where
  mempty = repeatV mempty

zipWithV :: KnownNat n => (a -> b -> c) -> V n a -> V n b -> V n c
zipWithV _ Nil      _      = Nil
zipWithV f (a:.as) (b:.bs) = f a b :. zipWithV f as bs

instance (KnownNat n, Eq a) => Eq (V n a) where
  (==) = (foldr (&&) True .) . zipWithV (==)

deduceZero :: KnownNat n => (1 <=? n) :~: 'False -> (n :~: 0)
deduceZero = unsafeCoerce

repeatV :: forall a n. KnownNat n => a -> V n a
repeatV a = unfold id a

instance (KnownNat n, Storable a) => Storable (V n a) where
  sizeOf :: V n a -> Int
  sizeOf _ = n * sizeOf (undefined :: a)
    where n = fromInteger $ natVal (Proxy @n)

  alignment _ = alignment (undefined :: a)
  
  poke ptr u = go u 0#
    where go :: KnownNat m => V m a -> Int# -> IO()
          go Nil     _  = pure ()
          go (a :. v) n# = do
              pokeElemOff (castPtr ptr) (I# n#) a
              go v (n# +# 1#)
  
  peek :: Ptr (V n a) -> IO (V n a)
  peek ptr = sequence . fmap (peekElemOff (castPtr ptr)) $ ixVec
      where ixVec :: V n Int
            ixVec = unfold pred (fromInteger $ natVal (Proxy @n))

        

dfoldrV :: forall n a b. KnownNat n => (forall k. KnownNat k => a -> b k -> b (k+1)) -> b 0 -> V n a -> b n
dfoldrV f d = go
  where go :: KnownNat m => V m a -> b m
        go Nil    = d
        go (a:.as) = f a (go as)

unfold :: forall n a. KnownNat n => (a -> a) -> a -> V n a
unfold f a = case (Proxy @1) %<=? (Proxy @n) of
                   LE Refl   -> let b = f a in b :. (unfold f b :: V (n-1) a)
                   NLE nle _ -> case deduceZero nle of
                                     Refl -> Nil

infixl 6 <++>

(<++>) :: (KnownNat n, KnownNat m) => V n a -> V m a -> V (n+m) a
(<++>) Nil      v = v
(<++>) (a:.Nil) v = a :. v
(<++>) (a:.as)   v = a :. (as <++> v)

instance KnownNat n => Applicative (V n) where
  pure = repeatV
  Nil      <*>  _        = Nil
  (f :. fs) <*> (a :. as) = f a :. fs <*> as

infixl 6 ^+^, ^-^
infixl 7 ^.^, ^×^
infix  8 ^*, *^

class (Num (Scalar v), RealFloat (Scalar v), KnownNat (Dim v)) => Vector v where
  type Scalar v :: Type
  type Dim v :: Nat
  (^+^), (^-^) :: v -> v -> v
  (*^)  :: Scalar v -> v -> v
  (^*)  :: v -> Scalar v -> v
  (^*)  = flip (*^)
  (^-^) x y = x ^+^ ((-1) *^ y)
  components :: v -> V (Dim v) (Scalar v)
  fromComponents :: V (Dim v) (Scalar v) -> v


pattern V2 x y = x :. y :. Nil
pattern V3 x y z = x :. y :. z :. Nil
pattern V4 x y z w = x :. y :. z :. w :. Nil

instance (Num a, RealFloat a, KnownNat n) => Vector (V n a) where
  type Scalar (V n a) = a
  type Dim (V n a) = n
  (^+^) = zipWithV (+)
  (^-^) = zipWithV (-)
  (*^) = fmap . (*)
  components = id
  fromComponents = id

class Vector v => Inner v where
  (^.^), dot :: v -> v -> Scalar v
  dot   = (^.^)

instance (Num a, RealFloat a, KnownNat n) => Inner (V n a) where
  (^.^) = (foldr (+) 0 .) . zipWithV (*)

class Inner v => Cross v where
  (^×^), cross :: v -> v -> v
  cross = (^×^)

instance (Num a, RealFloat a) => Cross (V 3 a) where
  (x :. y :. z :. Nil) ^×^ (x' :. y' :. z' :. Nil) = a :. b :. c :. Nil
    where a = y * z' - z * y'
          b = z * x' - x * z'
          c = x * y' - y * x'

angle :: Inner v => v -> v -> Scalar v
angle x y = asin $ dot (normalise x) (normalise y)

norm :: Inner v => v -> Scalar v
norm d = sqrt $ squaredNorm d

squaredNorm :: Inner v => v -> Scalar v
squaredNorm d = dot d d

sqdist :: Inner v => v -> v -> Scalar v
sqdist x y = squaredNorm (x ^-^ y)

distance :: Inner v => v -> v -> Scalar v
distance x y = norm (x ^-^ y)

along :: Vector v => Scalar v -> v -> v -> v
along d x y = (1-d) *^ x ^+^ d *^ y

normalise :: Inner v => v -> v
normalise d = case norm d of
                   0  -> d
                   nm -> (1 / nm) *^ d

reflect :: Inner v => v -> v -> v
reflect  d n = reflect' d (normalise n)

reflect' :: Inner v => v -> v -> v
reflect' d n = d ^-^ (2 * dot d n) *^ n -- assumes n is normalised

-- |Projects the first argument onto the second.
proj :: Inner v => v -> v -> v
proj x y = projC x y *^ y

projC :: Inner v => v -> v -> Scalar v
projC x y = case sqNm of
                 0  -> 0
                 sq -> dot x y / sq
  where sqNm = dot y y

isOrthogonal :: Inner v => v -> v -> Bool
isOrthogonal v w
  | dot v w == 0 = True
  | otherwise    = False

-- |Gram-Schmidt algorithm.
gramSchmidt :: Inner v => [v] -> [v]
gramSchmidt []     = []
gramSchmidt (x:xs) = x' : gramSchmidt (map (\v -> v ^-^ proj v x') xs)
  where x' = normalise x

rotateAroundAxis :: Cross v => v -> Scalar v -> v -> v
rotateAroundAxis n theta v = cos theta *^ v ^+^ sin theta *^ (n ^×^ v)


type M n m a = V n (V m a)

zero :: (KnownNat n, KnownNat m, Num a) => M n m a
zero = repeatV (repeatV 0)

identity :: forall n c. (KnownNat n, Num c) => M n n c
identity = case (Proxy :: Proxy 1) %<=? (Proxy :: Proxy n) of
                 LE Refl   -> (1 :. repeatV 0) :. fmap (0:.) (identity :: M (n-1) (n-1) c)
                 NLE nle _ -> case deduceZero nle of
                                   Refl -> Nil


newtype WrappedMatrix m n a k = WrappedMatrix { wrappedMatrix :: M m (n+k) a }

addCol' :: (KnownNat m, KnownNat n, KnownNat k) => WrappedMatrix m n a k -> V m a -> WrappedMatrix m n a (k+1)
addCol' WrappedMatrix { wrappedMatrix = mat } col = WrappedMatrix { wrappedMatrix = zipWithV (:.) col mat }

addCol :: forall a m n. (KnownNat m, KnownNat n) => M m n a -> V m a -> M m (n+1) a
addCol mat v = wrappedMatrix $ addCol' (WrappedMatrix { wrappedMatrix = mat } :: WrappedMatrix m n a 0) v

addCols :: (KnownNat m, KnownNat n, KnownNat l) => M m n a -> V l (V m a) -> M m (n+l) a
addCols mat cols = wrappedMatrix $ dfoldrV (flip addCol') (WrappedMatrix mat) cols

rowMatrix :: KnownNat n => V n a -> M 1 n a
rowMatrix = (:. Nil)

columnMatrix :: KnownNat n => V n a -> M n 1 a
columnMatrix = fmap (:. Nil)

transpose :: (KnownNat n, KnownNat m) => M n m a -> M m n a
transpose rows = repeatV Nil `addCols` rows 

blockSum :: forall m1 m2 n1 n2 a. (KnownNat m1, KnownNat m2, KnownNat n1, KnownNat n2, Num a) 
         => M m1 n1 a -> M m2 n2 a -> M (m1+m2) (n1+n2) a
blockSum mat1 mat2 = fmap (<++> repeatV 0) mat1 <++> fmap (repeatV 0 <++>) mat2

fromColumns :: forall l m a. (KnownNat m, KnownNat l) => V l (V m a) -> M m l a
fromColumns = addCols ( repeatV Nil :: M m 0 a)

infixl 6 !+!, !-!
infixl 7 !*!
infix  8 !*^

(!+!) :: (KnownNat n, KnownNat m, Num a) => M n m a -> M n m a -> M n m a
(!+!) = liftA2 (liftA2 (+))

(!-!) :: (KnownNat n, KnownNat m, Num a) => M n m a -> M n m a -> M n m a
(!-!) = liftA2 (liftA2 (-))

(!*!) :: (KnownNat l, KnownNat n, KnownNat m, Num a) => M m n a -> M n l a -> M m l a
m1 !*! m2 = fmap (\row -> fmap (sum . zipWithV (*) row) (transpose m2)) m1

(!*^) :: (KnownNat n, KnownNat m, Num a) => M n m a -> V m a -> V n a
m !*^ v = fmap (\row -> sum $ zipWithV (*) row v) m


lookAt :: (Num a, RealFloat a) => V 3 a -> V 3 a -> V 3 a -> M 4 4 a
lookAt eye centre up 
  = V4 (         xa <++> (-xd :. Nil) ) 
       (         ya <++> (-yd :. Nil) )
       ( (-1) *^ za <++> ( zd :. Nil) )
       ( V4 0 0 0 1 )
  where za = normalise (centre ^-^ eye)
        xa = normalise (za `cross` up)
        ya = xa `cross` za
        xd = xa ^.^ eye
        yd = ya ^.^ eye
        zd = za ^.^ eye
  
perspective :: (Num a, RealFloat a) => a -> a -> a -> a -> M 4 4 a
perspective fovy aspect near far
  = V4 ( V4 x 0   0  0 )
       ( V4 0 y   0  0 )
       ( V4 0 0   z  w )
       ( V4 0 0 (-1) 0 )
  where tanHalfFovy = tan (fovy / 2)
        x = 1 / (aspect * tanHalfFovy)
        y = 1 / tanHalfFovy
        z = (near + far) / (near - far)
        w = 2 * near * far / (near - far)

translation :: forall n a. (KnownNat n, Num a, RealFloat a) => V n a -> M (n+1) (n+1) a
translation v = ( zipWithV (<++>) identity ( fmap (:. Nil) v ) ) <++> ( ( repeatV 0 <++> (1 :. Nil) ) :. Nil)
