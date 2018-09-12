{-# language DataKinds                  #-}
{-# language DeriveGeneric              #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase                 #-}
{-# language PatternSynonyms            #-}
{-# language RecordWildCards            #-}

module Quake3.Input where

-- base
import Data.Coerce ( coerce )
import Data.Monoid ( Any(..), Sum(..) )
import Foreign.C
import GHC.Generics ( Generic )

-- generic-deriving
import Generics.Deriving.Monoid ( memptydefault, mappenddefault )

-- linear
import Math.Linear( V(..), pattern V2, pattern V3 )

-- sdl2
import qualified SDL
import qualified SDL.Event

newtype Quit = Quit Any
  deriving (Eq, Show, Semigroup, Monoid)

quit :: Quit
quit = coerce True

data KBM = KBM
  { keys      :: [SDL.Scancode]
  , mousePos  :: V 2 Foreign.C.CFloat
  , mouseRel  :: V 2 Foreign.C.CFloat
  , quitEvent :: Quit
  }

defaultKBM :: KBM
defaultKBM = KBM [] (V2 0 0) (V2 0 0) mempty

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

-----------------------------------------
-- Processing of directional information


strafeDir :: SDL.Scancode -> V 3 Foreign.C.CFloat
strafeDir SDL.ScancodeW = V3   0   0 (-5)
strafeDir SDL.ScancodeS = V3   0   0   5
strafeDir SDL.ScancodeA = V3 (-5)  0   0
strafeDir SDL.ScancodeD = V3   5   0   0
strafeDir _             = V3   0   0   0

strafe :: [SDL.Scancode] -> V 3 (Sum Foreign.C.CFloat)
strafe keys = foldMap (fmap Sum . strafeDir) keys

onSDLInput :: KBM -> SDL.EventPayload -> KBM
onSDLInput kbm SDL.QuitEvent = kbm { quitEvent = quit }
onSDLInput kbm (SDL.KeyboardEvent ev)
  = let keyCode = SDL.keysymScancode (SDL.keyboardEventKeysym ev)
    in case SDL.keyboardEventKeyMotion ev of
         SDL.Pressed  -> kbm { keys = keyCode : filter (/= keyCode) (keys kbm) }
         SDL.Released -> kbm { keys =           filter (/= keyCode) (keys kbm) }
onSDLInput kbm (SDL.MouseMotionEvent ev) 
  = kbm { mousePos = fmap ((/100) . fromIntegral) (V2 px py)
        , mouseRel = fmap ((/100) . fromIntegral) (V2 rx ry)
        }
    where 
      SDL.P (SDL.V2 px py) = SDL.mouseMotionEventPos       ev
      SDL.V2        rx ry  = SDL.mouseMotionEventRelMotion ev
onSDLInput kbm _ = kbm

-----------------------------------------

interpretKBM :: KBM -> Action
interpretKBM KBM{..} =
  let impulse    = strafe keys
      escape     = foldMap
                      ( \case { SDL.ScancodeEscape -> quit; _ -> mempty } )
                      keys
      quitAction = quitEvent <> escape
      rotate     = fmap Sum mouseRel
  in Action{..}