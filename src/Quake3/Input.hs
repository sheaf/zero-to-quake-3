{-# language Arrows                     #-}
{-# language DataKinds                  #-}
{-# language DeriveGeneric              #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase                 #-}
{-# language PatternSynonyms            #-}
{-# language RecordWildCards            #-}

module Quake3.Input where

-- base
import Control.Arrow
import Control.Monad.IO.Class ( MonadIO )
import Data.Coerce ( coerce )
import Data.Foldable ( foldl' )
import Data.Monoid ( Any(..), Sum(..) )
import Foreign.C
import GHC.Generics ( Generic )

-- dunai
import qualified Data.MonadicStreamFunction as D
import qualified Control.Monad.Trans.MSF as D

-- generic-deriving
import Generics.Deriving.Monoid ( memptydefault, mappenddefault )

-- transformers
import Control.Monad.Trans.Except( ExceptT )

-- zero-to-quake-3
import Math.Linear( V(..), pattern V2, pattern V3 )

-- sdl2
import qualified SDL
import qualified SDL.Event

newtype Quit = Quit Any
  deriving (Eq, Show, Semigroup, Monoid)

quit :: Quit
quit = coerce True

data Input = Input
  { keys      :: [SDL.Scancode]
  , mousePos  :: V 2 Foreign.C.CFloat
  , mouseRel  :: V 2 Foreign.C.CFloat
  , quitEvent :: Quit
  }

defaultInput :: Input
defaultInput = Input [] (V2 0 0) (V2 0 0) mempty

data Action = Action
  { impulse    :: V 3 ( Sum Foreign.C.CFloat )
  , rotate     :: V 2 ( Sum Foreign.C.CFloat )
  , quitAction :: Quit
  } deriving(Generic)

instance Semigroup Action where
  (<>) = mappenddefault
instance Monoid Action where
  mempty = memptydefault
  mappend = mappenddefault

shouldQuit :: Action -> Bool
shouldQuit action = quitAction action == quit

-----------------------------------------
-- Processing of directional information


strafeDir :: SDL.Scancode -> V 3 Foreign.C.CFloat
strafeDir SDL.ScancodeW = V3   0   0 (-5)
strafeDir SDL.ScancodeS = V3   0   0   5
strafeDir SDL.ScancodeA = V3 (-5)  0   0
strafeDir SDL.ScancodeD = V3   5   0   0
strafeDir _             = V3   0   0   0

strafe :: [SDL.Scancode] -> V 3 (Sum Foreign.C.CFloat)
strafe = foldMap (fmap Sum . strafeDir)

onSDLInput :: Input -> SDL.EventPayload -> Input
onSDLInput input SDL.QuitEvent = input { quitEvent = quit }
onSDLInput input (SDL.KeyboardEvent ev)
  = let keyCode = SDL.keysymScancode (SDL.keyboardEventKeysym ev)
    in case SDL.keyboardEventKeyMotion ev of
         SDL.Pressed  -> input { keys = keyCode : filter (/= keyCode) (keys input) }
         SDL.Released -> input { keys =           filter (/= keyCode) (keys input) }
onSDLInput input (SDL.MouseMotionEvent ev) 
  = input { mousePos = fmap ((/100) . fromIntegral) (V2 px py)
          , mouseRel = fmap ((/100) . fromIntegral) (V2 rx ry)
          }
    where 
      SDL.P (SDL.V2 px py) = SDL.mouseMotionEventPos       ev
      SDL.V2        rx ry  = SDL.mouseMotionEventRelMotion ev
onSDLInput input _ = input

-----------------------------------------
-- signal functions

inputSF :: MonadIO m => D.MSF m () Input
inputSF = D.arrM_ ( map SDL.eventPayload <$> SDL.pollEvents )
              >>> D.feedback defaultInput updWithZeroing
  where updWithZeroing :: Monad n 
                   => D.MSF n 
                        ( [SDL.EventPayload], Input ) 
                        ( Input             , Input ) 
        updWithZeroing = proc ( payloads, input ) -> do
          zeroedInput <- arr ( \input -> input { mouseRel = V2 0 0 } ) 
                      -< input
          input'      <- arr ( \(pays, ins) -> foldl' onSDLInput ins pays ) 
                      -< ( payloads, zeroedInput )
          returnA     -< ( input', input' )


interpretInput :: Input -> Action
interpretInput Input{..} =
  let impulse    = strafe keys
      escape     = foldMap
                      ( \case { SDL.ScancodeEscape -> quit; _ -> mempty } )
                      keys
      quitAction = quitEvent <> escape
      rotate     = fmap Sum mouseRel
  in Action{..}

interpretSF :: Monad m => D.MSF (ExceptT () m) Input Action
interpretSF = D.untilE ( arr interpretInput ) 
            $ arr ( \action -> if shouldQuit action
                               then Just () 
                               else Nothing
                  )

actionSF :: MonadIO m => D.MSF (ExceptT () m) () Action
actionSF =  D.liftMSFTrans inputSF >>> interpretSF
