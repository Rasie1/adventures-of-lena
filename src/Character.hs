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
                 -- , currentPosition = cp
                 , falling = False } = c { jumping = False
                                         , falling = True
                                         -- , currentPosition = cp `pointPlus` (0, -1)
                                         , currentVelocity = (dx, dy - jy * 10) }

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

updateCollisions :: World -> Character -> Character
updateCollisions World { level = Level { tiles = t } }
        c@Character { radius = r
                    , currentPosition = (x, y)
                    , falling = isFalling
                    , currentVelocity = (dx, dy) } = 
        c { currentPosition = (newx, newy) 
          , currentVelocity = (newdx, newdy)
          , falling = newFalling }
        
        where
          xl = x - r
          xr = x + r
          yt = y - r
          yb = y + r
          hitsb = isSolid (t ! (toCoord x, toCoord yb))
          hitst = isSolid (t ! (toCoord x, toCoord yt))
          hitsl = isSolid (t ! (toCoord xl, toCoord y))
          hitsr = isSolid (t ! (toCoord xr, toCoord y))
          toCoord = floor . (+0.0)
          flatten = fromIntegral . toCoord 
          newx = if hitsr then flatten xr - r
                          else if hitsl then flatten (xl + 0.5) + r
                                        else x
          newy = if hitsb then flatten yb - r
                          else if hitst then flatten (yt + 0.5) + r
                                        else y
          newdx = if hitsl && dx < 0.0 then 0
                                       else if hitsr && dx > 0.0 then 0
                                                                 else dx
          newdy = if hitst && dy < 0.0 then 0
                                       else if hitsb && dy > 0.0 then 0
                                                                 else dy
          newFalling = not hitsb


fall :: World -> Character -> Character
fall World { level = Level { tiles = t } }
        c@Character { radius = r
                    , falling = isFalling
                    , currentVelocity = (dx, dy) } = 
    c { currentVelocity = (dx, dy + characterGravity) } 

characterGravity = 0.3

updateCharacter :: DeltaTime -> World -> Character -> Maybe Character
updateCharacter dt world ch = Just 
                           . updateCollisions world
                           . updatePosition dt
                           . fall world
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
