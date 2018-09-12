{-# language DataKinds                  #-}
{-# language GADTs                      #-}
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

-- linear
import Math.Linear ( V(..), (.:), (^+^), pattern V2, pattern V3 )
import qualified Math.Quaternion as Quaternion

-- zero-to-quake-3
import Quake3.Input ( Action(..), Quit )


data Quake3State = Quake3State
  { cameraPosition :: V 3 Foreign.C.CFloat
  , cameraAngles :: V 2 Foreign.C.CFloat
  , shouldQuit :: Quit
  }

step :: Quake3State -> Action -> Quake3State
step Quake3State { .. }
    Action {..} =
  let
    newAngles@(V2 x y) = cameraAngles ^+^ liftA2 (*) ( fmap getSum rotate ) ( V2 (-1) 1 )
    orientation = Quaternion.axisAngle (V3 0 1 0) x * Quaternion.axisAngle (V3 1 0 0) y
  in
  Quake3State
    { cameraPosition =
        cameraPosition ^+^ Quaternion.rotate orientation ( fmap getSum impulse )
    , cameraAngles = newAngles
    , shouldQuit = quitAction
    }


initial :: Quake3State
initial =
  Quake3State (V3 0 0 0) (V2 0 0) mempty
