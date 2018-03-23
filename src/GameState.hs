module GameState where

import World
import Camera
import Common
import Types
import System.Clock

mkGameState :: World -> TimeSpec -> DigitsTextures -> GameState
mkGameState w time dtex = GameState
  { world = w
  , currentTime = time

  , camera = Camera 
       { cameraPosition = (0, 0)
       , oldCameraEdge  = (0, 0)
       , armLength      = (0, 4)
       , oldPivot       = (0, 0)
       , pivotOffset    = (0, -2)
       }

  , framesSinceLastFPSPrint = 0
  , lastFPSPrintTime = time
  , shutdown = False
  , digitsTextures = dtex
  }
