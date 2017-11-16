module Character where

import Common
import Camera
import Drawable
import Types
import Level
import qualified SDL
import Data.Array

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
                                         , currentSpeed = (dx, dy - jy) }

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

updatePosition :: DeltaTime -> Character -> Character
updatePosition dt c@Character { currentSpeed = (dx, dy)
                              , currentPosition = (x, y) } = 
    c { currentPosition = (x + dx * dt, y + dy * dt) }

fall :: DeltaTime -> World -> Character -> Character
fall dt World { level = Level { tiles = t } }
        c@Character { currentPosition = (x, y)
                    , currentSpeed = (dx, dy) } = 
    if pos `elem` indices t 
        then if isSolid (t ! pos)
                 then stop
                 else c
        else stop
    where stopy = c { currentSpeed = (dx, 0), falling = False }
          stopx = c { currentSpeed = (0, dy), falling = False }
          stop  = c { currentSpeed = (0, 0) , falling = False  }
          pos = (floor (x + dx * dt), floor (y + dy * dt))

characterGravity = 0.098

applyGravity :: Character -> Character
applyGravity c@Character { currentSpeed = (dx, dy) } = 
    c { currentSpeed = (dx, dy + characterGravity) }

updateCharacter :: DeltaTime -> World -> Character -> Maybe Character
updateCharacter dt world ch = Just . updatePosition dt
                           . fall dt world
                           . applyGravity
                           -- . activate world
                           -- . attack world
                           . jump 
                           . move $ ch

instance Drawable Character where
    render camera renderer (texture, ti) character = do
        renderSprite (currentPosition character) camera
        where
          tileWidth :: Double
          tileWidth = (fromIntegral $ SDL.textureWidth ti) / 24
          tileRect = mkRect 0 0 tileWidth tileWidth

          getTilesheetCoords :: (Num a) => (a, a)
          getTilesheetCoords = (192, 192)

          renderSprite (x, y) camera
            = SDL.copy renderer texture
                (Just $ floor <$> moveTo getTilesheetCoords tileRect)
                (Just $ floor <$> applyCamera camera (moveTo (x * tileWidth, y * tileWidth) tileRect))
