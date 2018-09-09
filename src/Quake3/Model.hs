{-# language DataKinds                  #-}
{-# language DeriveGeneric              #-}
{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms            #-}
{-# language RecordWildCards            #-}

module Quake3.Model
  ( Quit(..)
  , Action(..)
  , Quake3State(..)
  , initial
  , step
  ) where

-- base
import Control.Applicative ( liftA2 )
import Data.Monoid ( Any(..), Sum(..) )
import qualified Foreign.C
import GHC.Generics ( Generic )

-- generic-deriving
import Generics.Deriving.Monoid ( memptydefault, mappenddefault )

-- linear
import Math.Linear ( V(..), (.:), (^+^), pattern V2, pattern V3 )
import qualified Math.Quaternion as Quaternion


data Quake3State = Quake3State
  { cameraPosition :: V 3 Foreign.C.CFloat
  , cameraAngles :: V 2 Foreign.C.CFloat
  , shouldQuit :: Quit
  }

newtype Quit = Quit Any
  deriving (Eq, Show, Semigroup, Monoid)

data Action = Action
  { impulse :: V 3 ( Sum Foreign.C.CFloat )
  , rotate :: V 2 ( Sum Foreign.C.CFloat )
  , quitAction :: Quit
  } deriving(Generic)


instance Semigroup Action where
  (<>) = mappenddefault
instance Monoid Action where
  mempty = memptydefault
  mappend = mappenddefault

step :: Quake3State -> Action -> Quake3State
step Quake3State
     { cameraAngles = V2 phi_x phi_y
     , ..
     }
    Action{..} =
  let
    orientation =
      Quaternion.axisAngle ( V3 0 1 0 ) ( phi_x )
        * Quaternion.axisAngle (V3 1 0 0 ) ( phi_y )
  in
  Quake3State
    { cameraPosition =
        cameraPosition ^+^ Quaternion.rotate orientation ( fmap getSum impulse )
    , cameraAngles =
        V2 phi_x phi_y ^+^ liftA2 (*) ( fmap getSum rotate ) ( V2 (-1) 1 )
    , shouldQuit = quitAction
    }


initial :: Quake3State
initial =
  Quake3State (V3 0 0 0) (V2 0 0) mempty
