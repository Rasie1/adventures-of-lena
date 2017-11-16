module Character where

import Common
import Camera
import Drawable
import Actor
import Level
import Intent
import qualified SDL

data Character = Character
    { moveSpeed  :: Double
    , radius     :: Double
    , inertia    :: Double
    , jumpHeight :: Double

    , currentPosition :: Position
    , currentSpeed    :: Speed

    , characterController :: Controller

    , moving    :: MovePosition
    , falling   :: Bool
    , using     :: Bool
    , attacking :: Bool
    , jumping   :: Bool
    }

data MovePosition = MovingLeft | MovingRight | NotMoving

data Controller = Controller
    { port :: Int,
      actions :: [Intent]
    }

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

instance Actor Character where
    act dt ch = Just . updatePosition
                           -- . fall world
                           -- . applyGravity
                           -- . activate world
                           -- . attack world
                           . jump 
                           . move $ ch

instance Drawable Character where
    render character camera renderer (texture, ti) = do
        renderTile 0 (-1) Sky camera
        where
          tileWidth :: Double
          tileWidth = (fromIntegral $ SDL.textureWidth ti) / 16
          tileRect = mkRect 0 0 tileWidth tileWidth

          getTilesheetCoords :: (Num a) => Tile -> (a, a)
          getTilesheetCoords Sky = (0, 0)
          getTilesheetCoords Grass = (64, 0)

          renderTile x y t camera
            = SDL.copy renderer texture
                (Just $ floor <$> moveTo (getTilesheetCoords t) tileRect)
                (Just $ floor <$> applyCamera camera (moveTo (fromIntegral x * tileWidth, fromIntegral y * tileWidth) tileRect))
