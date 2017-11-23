module GameState where

import World
import System.Clock
import Camera
import Common
import Types
import Data.Maybe
import Data.List
import System.IO.Unsafe

data GameState = GameState
  { world       :: World
  , currentTime :: TimeSpec

  , framesSinceLastFPSPrint :: Int
  , lastFPSPrintTime        :: TimeSpec

  , cameraPosition :: Position
  , oldCameraEdge  :: Position
  , armLength      :: Position
  , oldPivot       :: Position
  , pivotOffset    :: Position

  , shutdown :: Bool
  }

mkGameState :: World -> TimeSpec -> GameState
mkGameState w time = GameState
  { world = w
  , currentTime = time

  , cameraPosition = (0, 0)
  , oldCameraEdge  = (0, 0)
  , armLength      = (0, 4)
  , oldPivot       = (0, 0)
  , pivotOffset    = (0, -2)

  , framesSinceLastFPSPrint = 0
  , lastFPSPrintTime = time
  , shutdown = False
  }

updateCamera :: GameState -> GameState
updateCamera s@GameState { cameraPosition = (oldx, oldy)
                         , oldCameraEdge  = (edgex, edgey)
                         , oldPivot       = (pivotx, pivoty)
                         , armLength      = (lengthx, lengthy)
                         , pivotOffset    = pivotOffset } = 
    s { cameraPosition = newCombined
      , oldCameraEdge = newEdge
      , oldPivot = playerPosition }
    where playerPosition = pointPlus pivotOffset
                         . fromMaybe (0, 0)
                         . fmap currentPosition 
                         . find isPlayer 
                         . characters 
                         . world $ s
          newx = fst playerPosition
          newy = snd playerPosition

          isPlayer Character {} = True
          isPlayer _            = False

          xLengthExceed = abs (newx - edgex) > lengthx
          yLengthExceed = abs (newy - edgey) > lengthy
          newCombined   = ( if xLengthExceed then newx else oldx
                          , if yLengthExceed then newy else oldy
                          )
          newEdge =  ( if xLengthExceed 
                          then edgex + (newx - pivotx)
                          else edgex
                     , if yLengthExceed 
                          then edgey + (newy - pivoty)
                          else edgey
                     )
