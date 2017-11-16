module GameState where

import World
import System.Clock
import Camera
import Common
import Types

data GameState = GameState
  { world :: Maybe World
  , cameraPosition :: Camera
  , currentTime :: TimeSpec

  , framesSinceLastFPSPrint :: Int
  , lastFPSPrintTime :: TimeSpec
  }

mkGameState :: World -> TimeSpec -> GameState
mkGameState w time = GameState
  { world = Just w
  , currentTime = time
  , cameraPosition = (0, 0)
  , framesSinceLastFPSPrint = 0
  , lastFPSPrintTime = time
  }

quitGameState :: GameState -> GameState
quitGameState w = w { world = Nothing }
