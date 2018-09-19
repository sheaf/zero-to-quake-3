{-# language DataKinds                  #-}
{-# language GADTs                      #-}
{-# language LambdaCase                 #-}
{-# language PatternSynonyms            #-}
{-# language RecordWildCards            #-}

module Quake3.Model
  ( Action(..)
  , Quake3State(..)
  , simulationSF
  , initial
  ) where

-- base
import Control.Applicative ( liftA2 )
import Control.Arrow
import Data.List ( sortOn )
import Data.Maybe ( mapMaybe )
import Data.Monoid ( Sum(..) )
import Data.Ord ( Down(..) )
import qualified Foreign.C

-- dunai
import qualified Data.MonadicStreamFunction as D

-- zero-to-quake-3
import Math.Linear ( V(..), (^+^), applyM44, pattern V2, pattern V3 )
import qualified Math.Quaternion as Quaternion
import Quake3.Input ( Action(..) )
import Quake3.Constants ( simulationTickTime )

-- zero-to-quake3
import Math.Coordinates ( q3ToVk )
import qualified Quake3.Entity
import qualified Quake3.Entity.InfoPlayerDeathmatch as InfoPlayerDeathmatch


data Quake3State = Quake3State
  { cameraPosition :: V 3 Foreign.C.CFloat
  , cameraAngles   :: V 2 Foreign.C.CFloat
  }

scaleMovement :: V 2 Foreign.C.CFloat -> V 3 Foreign.C.CFloat
scaleMovement ( V2 x y )
  = fmap 
      ( * ( 500 * fromRational simulationTickTime ) )
      ( V3 x 0 (-y) )

step :: Action -> Quake3State -> Quake3State
step Action {..}
     Quake3State { .. } =
  let
    newAngles@(V2 x y) = cameraAngles ^+^ liftA2 (*) ( fmap getSum look ) ( V2 (-1) 1 )
    orientation = Quaternion.axisAngle (V3 0 1 0) x * Quaternion.axisAngle (V3 1 0 0) y
  in
  Quake3State
    { cameraPosition =
        cameraPosition ^+^ Quaternion.rotate 
                              orientation 
                              ( scaleMovement ( fmap getSum movement ) )
    , cameraAngles = newAngles
    }


initial :: [Quake3.Entity.Entity] -> Quake3State
initial initialEntities =
  let
    spawnPoints =
      sortOn
        ( Down . ( Just 1 == ) . InfoPlayerDeathmatch.spawnFlags )
        ( mapMaybe
            ( \case { Quake3.Entity.InfoPlayerDeathmatch info -> Just info; _ -> Nothing } )
            initialEntities
        )
    initialSpawnPoint =
      case spawnPoints of
        [] ->
          InfoPlayerDeathmatch.InfoPlayerDeathmatch
            { origin     = V3 0 0 0
            , angle      = 0
            , spawnFlags = Nothing
            }

        ( a : _ ) ->
          a
    initialPosition = applyM44 q3ToVk ( fromIntegral <$> InfoPlayerDeathmatch.origin initialSpawnPoint )
    initialAngle    = V2 
                        ( pi - degToRad -- TODO: remove this hacky adjustment
                          ( fromIntegral ( InfoPlayerDeathmatch.angle initialSpawnPoint ) )
                        )
                        0
  in Quake3State initialPosition initialAngle


degToRad :: Floating a => a -> a
degToRad x =
  x * ( pi / 180 )

-----------------------------------------
-- signal function

simulationSF :: Monad m => Quake3State -> D.MSF m Action Quake3State
simulationSF initialQ3State = D.feedback initialQ3State sim
  where sim :: Monad n
            => D.MSF n ( Action     , Quake3State ) 
                       ( Quake3State, Quake3State ) 
        sim = arr (uncurry step) >>^ ( \ s -> (s,s) )
