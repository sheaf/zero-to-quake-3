{-# language DataKinds         #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}

module Quake3.Entity.InfoPlayerDeathmatch ( InfoPlayerDeathmatch(..), parse ) where

-- base
import Control.Applicative ( optional )
import Control.Monad ( guard )


-- zero-to-quake3
import Math.Linear ( V(..) )
import qualified Quake3.BSP.Entities


data InfoPlayerDeathmatch = InfoPlayerDeathmatch
  { angle      :: Int
  , origin     :: V 3 Int
  , spawnFlags :: Maybe Int
  }
  deriving ( Show )


parse
  :: Quake3.BSP.Entities.EntityProperties
  -> Maybe InfoPlayerDeathmatch
parse props = do
  Quake3.BSP.Entities.getText "classname" props
    >>= guard . ( == "info_player_deathmatch" )

  angle <-
    Quake3.BSP.Entities.getInt "angle" props

  origin <-
    Quake3.BSP.Entities.getIntV3 "origin" props

  spawnFlags <-
    optional
      ( Quake3.BSP.Entities.getInt "spawnflags" props )

  return InfoPlayerDeathmatch{..}
