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
        c@Character { radius = r
                    , currentPosition = (x, y)
                    , falling = isFalling
                    , currentSpeed = (dx, dy) } = 
    applyXcollisions . applyYcollisions $ c
    where stopy d c@Character { currentSpeed = (dx, dy) } = 
              c { currentSpeed = (dx, d) }
          stopx d c@Character { currentSpeed = (dx, dy) } = 
              c { currentSpeed = (d, dy) }
          applyGravity c@Character { currentSpeed = (dx, dy) } = 
              c { currentSpeed = (dx, dy + characterGravity) 
                , falling = True }
          stand c = c { falling = False }

          nextX = x + dx * dt 
          nextY = y + dy * dt
          nextXl = nextX - r
          nextXr = nextX + r
          nextYt = nextY - r + characterGravity
          nextYb = nextY + r + characterGravity

          alignDistance x = ((fromIntegral . floor $ x) - x) / dt

          bumpedxl = checkBump (nextXl, y)
          bumpedxr = checkBump (nextXr, y)
          bumpedyt = checkBump (x, nextYt)
          bumpedyb = checkBump (x, nextYb)
          bumpxl = if bumpedxl
                        then stopx (alignDistance nextXl)
                        else id
          bumpxr = if bumpedxr
                        then stopx (alignDistance nextXr) 
                        else id
          bumpyt = applyGravity .
                     if bumpedyt
                        then stopy (alignDistance nextYt)
                        else id
          bumpyb = if bumpedyb
                        then stand . stopy (alignDistance nextYb)
                        else applyGravity
          applyXcollisions = if bumpedxl then bumpxl
                                         else if bumpedxr then bumpxr
                                                          else id
          applyYcollisions = if bumpedyt then bumpyt
                                         else if bumpedyb then bumpyb
                                                          else applyGravity

          checkBump (x, y) = isSolid (t ! (floor x, floor y)) 

characterGravity = 0.2

updateCharacter :: DeltaTime -> World -> Character -> Maybe Character
updateCharacter dt world ch = Just . updatePosition dt
                           . fall dt world
                           -- . activate world
                           -- . attack world
                           . jump 
                           . move $ ch

instance Drawable Character where
    render camera renderer (texture, ti) character = do
        renderSprite pos camera
        where
          tileWidth :: Double
          tileWidth = (fromIntegral $ SDL.textureWidth ti) / 24
          tileRect = mkRect 0 0 tileWidth tileWidth

          pos = currentPosition character `pointPlus` (- radius character,
                                                       - radius character)

          getTilesheetCoords :: (Num a) => (a, a)
          getTilesheetCoords = (192, 192)

          renderSprite (x, y) camera
            = SDL.copy renderer texture
                (Just $ floor <$> moveTo getTilesheetCoords tileRect)
                (Just $ floor <$> applyCamera camera (moveTo (x * tileWidth, y * tileWidth) tileRect))
