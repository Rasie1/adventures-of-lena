module Bot where


import Common
import Camera
import Drawable
import Types
import Level
import qualified SDL
import Data.Array

simpleMoveBot :: Bot
simpleMoveBot _
          World { level = Level { tiles = t } }
          c@Character { radius = r
                      , currentPosition = (x, y)
                      , falling = isFalling
                      , currentVelocity = (dx, dy) 
                      , moving = m } =
    return $ case (hitsl, hitsr, m) of
                (True, False, _) -> MoveRight
                (False, True, _) -> MoveLeft

                (False, False, MovingLeft) -> MoveLeft
                otherwise          -> MoveRight
        where
          xl = x - r - 0.1
          xr = x + r
          hitsl = isSolid (t ! (toCoord xl, toCoord y))
          hitsr = isSolid (t ! (toCoord xr, toCoord y))
          toCoord = floor

noBot :: Bot
noBot _ _ _ = []
