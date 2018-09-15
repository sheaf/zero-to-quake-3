{-# language DataKinds                  #-}
{-# language GADTs                      #-}
{-# language PatternSynonyms            #-}
{-# language RecordWildCards            #-}

module Quake3.Model
  ( Action(..)
  , Quake3State(..)
  , simulationSF
  ) where

-- base
import Control.Applicative ( liftA2 )
import Control.Arrow
import Data.Monoid ( Sum(..) )
import qualified Foreign.C

-- dunai
import qualified Data.MonadicStreamFunction as D

-- zero-to-quake-3
import Math.Linear ( V(..), (^+^), pattern V2, pattern V3 )
import qualified Math.Quaternion as Quaternion
import Quake3.Input ( Action(..) )


data Quake3State = Quake3State
  { cameraPosition :: V 3 Foreign.C.CFloat
  , cameraAngles   :: V 2 Foreign.C.CFloat
  }

step :: Action -> Quake3State -> Quake3State
step Action {..}
     Quake3State { .. } =
  let
    newAngles@(V2 x y) = cameraAngles ^+^ liftA2 (*) ( fmap getSum rotate ) ( V2 (-1) 1 )
    orientation = Quaternion.axisAngle (V3 0 1 0) x * Quaternion.axisAngle (V3 1 0 0) y
  in
  Quake3State
    { cameraPosition =
        cameraPosition ^+^ Quaternion.rotate orientation ( fmap getSum impulse )
    , cameraAngles = newAngles
    }


initial :: Quake3State
initial =
  Quake3State (V3 0 0 0) (V2 0 0)

-----------------------------------------
-- signal function

simulationSF :: Monad m => D.MSF m Action Quake3State
simulationSF = D.feedback initial sim
  where sim :: Monad n 
            => D.MSF n ( Action     , Quake3State ) 
                       ( Quake3State, Quake3State ) 
        sim = arr (uncurry step) >>^ ( \ s -> (s,s) )
