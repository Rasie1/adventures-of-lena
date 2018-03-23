module Camera where
import qualified SDL
import Common
import Types
import Data.Maybe
import Data.List

updateCamera :: GameState -> GameState
updateCamera s@GameState { camera = cam@Camera
                            { cameraPosition = (oldx, oldy)
                            , oldCameraEdge  = (edgex, edgey)
                            , oldPivot       = (pivotx, pivoty)
                            , armLength      = (lengthx, lengthy)
                            , pivotOffset    = pivotOffset } } = 
    s { camera = cam { cameraPosition = newCombined
                     , oldCameraEdge = newEdge
                     , oldPivot = playerPosition } }
    where playerPosition = pointPlus pivotOffset
                         . currentPosition 
                         . playerCharacter 
                         . world $ s
          newx = fst playerPosition
          newy = snd playerPosition

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


applyCamera :: ScreenSize -> Double -> Camera -> SDL.Rectangle Double -> SDL.Rectangle Double
applyCamera (screenx, screeny) 
             unitSize 
             Camera { cameraPosition = (x, y) } = 
    moveBy ((-x * unitSize,
             -y * unitSize) `pointPlus` (screenx / 2,
                                         screeny / 2))
