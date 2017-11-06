module GameState where

import World
import System.Clock
import Camera

data GameState = GameState
  { exiting :: Bool

  , world :: World
  , cameraPosition :: Camera
  , currentTime :: TimeSpec

  , framesSinceLastFPSPrint :: Int
  , lastFPSPrintTime :: TimeSpec
  }

mkGameState :: World -> TimeSpec -> GameState
mkGameState w time = GameState
  { exiting = False
  , world = w
  , currentTime = time
  , cameraPosition = (0, 0)
  , framesSinceLastFPSPrint = 0
  , lastFPSPrintTime = time
  }

type DeltaTime = Double

idleGameState :: GameState -> GameState
idleGameState = id

quitGameState :: GameState -> GameState
quitGameState w = w { exiting = True }
