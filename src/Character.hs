module Character where

import Common
import Camera
import Drawable
import Types
import Level
import qualified SDL
import Data.Array

applyVelocity :: DeltaTime -> Position -> Velocity -> Position
applyVelocity dt (posx, posy) (spdx, spdy) = (posx + spdx * dt, posy + spdy * dt)

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
                 , currentVelocity = (dx, dy)
                 , jumpPower       = jy
                 , currentPosition = cp
                 , falling = False } = c { jumping = False
                                         , falling = True
                                         , currentPosition = cp `pointPlus` (0, -1)
                                         , currentVelocity = (dx, dy - jy) }

move :: Character -> Character
move c@Character { moving = MovingLeft
                 , currentVelocity = (dx, dy)
                 , moveVelocity = m } = c { currentVelocity = (-m, dy)}
move c@Character { moving = MovingRight 
                 , currentVelocity = (dx, dy)
                 , moveVelocity = m } = c { currentVelocity = (m, dy)}
move c@Character { moving = NotMoving 
                 , currentVelocity = (dx, dy)
                 , moveVelocity = m } = c { currentVelocity = (0, dy)}

updatePosition :: DeltaTime -> Character -> Character
updatePosition dt c@Character { currentVelocity = (dx, dy)
                              , currentPosition = (x, y) } = 
    c { currentPosition = (x + dx * dt, y + dy * dt) }

fall :: DeltaTime -> World -> Character -> Character
fall dt World { level = Level { tiles = t } }
        c@Character { radius = r
                    , currentPosition = (x, y)
                    , falling = isFalling
                    , currentVelocity = (dx, dy) } = 
    applyXcollisions . applyYcollisions $ c
    where stopy d c@Character { currentVelocity = (dx, dy) 
                              , currentPosition = (x, y) } = 
              c { currentVelocity = (dx, 0) 
                , currentPosition = (x, y + d) }
          stopx d c@Character { currentVelocity = (dx, dy) 
                              , currentPosition = (x, y)} = 
              c { currentVelocity = (0, dy)
                , currentPosition = (x + d, y) }
          applyGravity c@Character { currentVelocity = (dx, dy) } = 
              c { currentVelocity = (dx, dy + characterGravity) 
                , falling = True }
          stand c = c { falling = False }

          nextX = x + dx * dt 
          nextY = y + dy * dt
          nextXl = nextX - r
          nextXr = nextX + r
          nextYt = nextY - r + characterGravity
          nextYb = nextY + r + characterGravity

          alignDistance x old = ((fromIntegral . rounder $ x) - old)
                                where rounder = if x > old then floor
                                                           else ceiling

          bumpedxl = checkBump (nextXl, y)
          bumpedxr = checkBump (nextXr, y)
          bumpedyt = checkBump (x, nextYt)
          bumpedyb = checkBump (x, nextYb)
          bumpxl = if bumpedxl
                        then stopx (alignDistance nextXl (x - r))
                        else id
          bumpxr = if bumpedxr
                        then stopx (alignDistance nextXr (x + r)) 
                        else id
          bumpyt = applyGravity .
                     if bumpedyt
                        then stopy (alignDistance nextYt (y - r))
                        else id
          bumpyb = if bumpedyb
                        then stand . stopy (alignDistance nextYb (y + r))
                        else applyGravity
          applyXcollisions = if bumpedxl then bumpxl
                                         else if bumpedxr then bumpxr
                                                          else id
          applyYcollisions = if bumpedyt then bumpyt
                                         else if bumpedyb then bumpyb
                                                          else applyGravity

          checkBump (x, y) = isSolid (t ! (floor x, floor y)) 

characterGravity = 0.3

updateCharacter :: DeltaTime -> World -> Character -> Maybe Character
updateCharacter dt world ch = Just . updatePosition dt
                           . fall dt world
                           -- . activate world
                           -- . attack world
                           . jump 
                           . move $ ch

instance Drawable Character where
    render screen camera renderer (texture, ti) character = do
        renderSprite pos
        where
          tileWidth = (fromIntegral $ SDL.textureWidth ti) / 24
          tileRect = mkRect 0 0 tileWidth tileWidth

          pos = currentPosition character `pointPlus` (- radius character,
                                                       - radius character)

          getTilesheetCoords :: (Num a) => (a, a)
          getTilesheetCoords = (192, 192)

          renderSprite (x, y)
            = SDL.copy renderer texture src dst
              where src = Just $ floor <$> moveTo getTilesheetCoords tileRect
                    dst = Just $ floor <$> applyCamera screen tileWidth camera (moveTo dstPos tileRect)
                    dstPosX = x * tileWidth
                    dstPosY = y * tileWidth
                    dstPos = (dstPosX, dstPosY)
