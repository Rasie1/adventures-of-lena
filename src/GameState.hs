module GameState where

import World
import System.Clock
import Camera
import Common
import Types
import Data.Maybe
import Data.List

data GameState = GameState
  { world :: World
  , cameraPosition :: Camera
  , currentTime :: TimeSpec

  , framesSinceLastFPSPrint :: Int
  , lastFPSPrintTime :: TimeSpec

  , shutdown :: Bool
  }

mkGameState :: World -> TimeSpec -> GameState
mkGameState w time = GameState
  { world = w
  , currentTime = time
  , cameraPosition = (0, 0)
  , framesSinceLastFPSPrint = 0
  , lastFPSPrintTime = time
  , shutdown = False
  }

updateCamera :: GameState -> GameState
updateCamera c = c { cameraPosition = newCamera c }
    where newCamera = fromMaybe (0, 0) 
                    . fmap currentPosition 
                    . find isPlayer 
                    . characters 
                    . world
          isPlayer Character {} = True
          isPlayer _            = False
