module GameState where

import World
import System.Clock
import Camera

data GameState = GameState
  { exiting :: Bool
  , world :: World
  , currentTime :: TimeSpec
  , cameraPosition :: Camera
  }

mkGameState :: World -> TimeSpec -> GameState
mkGameState w time = GameState
  { exiting = False
  , world = w
  , currentTime = time
  , cameraPosition = (0, 0)
  }

type DeltaTime = Double

idleGameState :: GameState -> GameState
idleGameState = id

quitGameState :: GameState -> GameState
quitGameState w = w { exiting = True }
