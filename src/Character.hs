module Character where

import Common
import Camera
import Drawable
import Types
import Level
import qualified SDL

applySpeed :: DeltaTime -> Position -> Speed -> Position
applySpeed dt (posx, posy) (spdx, spdy) = (posx + spdx * dt, posy + spdy * dt)

cleanIntent :: Character -> Character
cleanIntent c = c { moving = NotMoving }

applyIntent :: Intent -> Character -> Character
applyIntent Idle      c = c
applyIntent MoveLeft  c = c { moving = MovingLeft  }
applyIntent MoveRight c = c { moving = MovingRight }
applyIntent Jump      c = c { jumping = True }
applyIntent Attack    c = c { attacking = True }
applyIntent Action    c = c
applyIntent Quit      c = c

jump :: Character -> Character
jump c@Character { jumping = False } = c
jump c@Character { jumping = True
                 , falling = True } = c { jumping = False }
jump c@Character { jumping = True
                 , currentSpeed = (dx, dy)
                 , jumpHeight = jy
                 , falling = False } = c { jumping = False
                                         , falling = True
                                         , currentSpeed = (dx, dy + jy) }

move :: Character -> Character
move c@Character { moving = MovingLeft
                 , currentSpeed = (dx, dy)
                 , moveSpeed = m } = c { currentSpeed = (-m, dy)}
move c@Character { moving = MovingRight 
                 , currentSpeed = (dx, dy)
                 , moveSpeed = m } = c { currentSpeed = (m, dy)}
move c@Character { moving = NotMoving 
                 , currentSpeed = (dx, dy)
                 , moveSpeed = m } = c { currentSpeed = (0, dy)}

updatePosition :: Character -> Character
updatePosition c@Character { currentSpeed = (dx, dy)
                           , currentPosition = (x, y) } = c { currentPosition = (x + dx, y + dy) }

updateCharacter :: Double -> World -> Character -> Maybe Character
updateCharacter dt world ch = Just . updatePosition
                           -- . fall world
                           -- . applyGravity
                           -- . activate world
                           -- . attack world
                           . jump 
                           . move $ ch

instance Drawable Character where
    render camera renderer (texture, ti) character = do
        renderTile 0 (-1) camera
        where
          tileWidth :: Double
          tileWidth = (fromIntegral $ SDL.textureWidth ti) / 16
          tileRect = mkRect 0 0 tileWidth tileWidth

          getTilesheetCoords :: (Num a) => (a, a)
          getTilesheetCoords = (192, 192)

          renderTile x y camera
            = SDL.copy renderer texture
                (Just $ floor <$> moveTo getTilesheetCoords tileRect)
                (Just $ floor <$> applyCamera camera (moveTo (fromIntegral x * tileWidth, fromIntegral y * tileWidth) tileRect))
