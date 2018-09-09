{-# language LambdaCase      #-}
{-# language PatternSynonyms #-}
{-# language RecordWildCards #-}

module Quake3.Input where

import Data.Coerce ( coerce )
import Data.Monoid ( Any(..), Sum(..) )

-- linear
import Math.Linear( pattern V2, pattern V3 )

-- sdl2
import qualified SDL
import qualified SDL.Event

-- zero-to-quake-3
import qualified Quake3.Model


eventToAction :: SDL.Event -> Quake3.Model.Action
eventToAction =
  eventPayloadToAction . SDL.Event.eventPayload


eventPayloadToAction :: SDL.EventPayload -> Quake3.Model.Action
eventPayloadToAction = \case
  SDL.Event.KeyboardEvent e ->
    keyboardEventToAction e

  SDL.Event.MouseMotionEvent e ->
    mouseMotionEventToAction e

  _ ->
    mempty


keyboardEventToAction :: SDL.KeyboardEventData -> Quake3.Model.Action
keyboardEventToAction SDL.Event.KeyboardEventData{..} | keyboardEventKeyMotion == SDL.Event.Pressed =
  Quake3.Model.Action
    { impulse =
        fmap Sum
          ( case SDL.keysymScancode keyboardEventKeysym of
              SDL.ScancodeW ->
                V3 0 0 (-0.1)

              SDL.ScancodeS ->
                V3 0 0 0.1

              SDL.ScancodeA ->
                V3 (-0.1) 0 0

              SDL.ScancodeD ->
                V3 0.1 0 0

              _ -> V3 0 0 0
          )
    , rotate =
        mempty
    , quitAction
       = case SDL.keysymScancode keyboardEventKeysym of
              SDL.ScancodeEscape -> coerce True
              _                  -> mempty
    }
keyboardEventToAction _ =
  mempty


mouseMotionEventToAction :: SDL.MouseMotionEventData -> Quake3.Model.Action
mouseMotionEventToAction SDL.MouseMotionEventData{..} =
  Quake3.Model.Action
    { impulse = mempty
    , rotate =
        let SDL.V2 mx my = fmap fromIntegral mouseMotionEventRelMotion
        in fmap ( Sum . ( / 100 ) ) $ V2 mx my
    , quitAction = mempty
    }
