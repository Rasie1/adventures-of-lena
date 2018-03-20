module Character where

import Common
import Camera
import Drawable
import Types
import Level
import qualified SDL
import Data.Array
import SpriteSheet

applyVelocity :: DeltaTime -> Position -> Velocity -> Position
applyVelocity dt (posx, posy) (spdx, spdy) = (posx + spdx * dt, posy + spdy * dt)

cleanIntent :: Character -> Character
cleanIntent c = c { moving = NotMoving }

applyIntent :: Intent -> Character -> Character
applyIntent Idle      c = c 
applyIntent Stop      c = c { moving = NotMoving }
applyIntent MoveLeft  c = c { moving = MovingLeft  }
applyIntent MoveRight c = c { moving = MovingRight }
applyIntent Jump      c = c { jumping = True }
applyIntent Attack    c = c { attacking = True }
applyIntent Action    c = c
applyIntent Quit      c = c

jump :: Character -> Character
jump c@Character { canJump = False } = c { jumping = False }
jump c@Character { jumping = True
                 , currentVelocity = (dx, dy)
                 , jumpPower       = jy
                 , canJump = True }  = c { jumping = False
                                         , falling = True
                                         , canJump = False
                                         -- , currentPosition = cp `pointPlus` (0, -1)
                                         , currentVelocity = (dx, dy - jy * 10) }
jump c = c

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

updateCollisions :: DeltaTime -> World -> Character -> Character
updateCollisions dt World { level = Level { tiles = t } }
        c@Character { radius = r
                    , currentPosition = (x, y)
                    , falling = isFalling
                    , currentVelocity = (dx, dy)
                    , canJump = oldCanJump } = 
        c { currentPosition = (newx, newy) 
          , currentVelocity = (newdx, newdy)
          , falling = newFalling
          , canJump = newCanJump
          , timeToJump = newTimeToJump }
        
        where
          xl = x - r
          xr = x + r
          yt = y - r
          yb = y + r
          hitsb = isSolid (getTile t (toCoord x, toCoord yb))
          hitst = isSolid (getTile t (toCoord x, toCoord yt))
          hitsl = isSolid (getTile t (toCoord xl, toCoord y))
          hitsr = isSolid (getTile t (toCoord xr, toCoord y))
          toCoord = floor
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
          defaultTimeToJump = 0.2
          newTimeToJump = case (isFalling, newFalling) of
                                (False, False) -> defaultTimeToJump
                                (False, True)  -> defaultTimeToJump
                                (True, True)   -> timeToJump c - dt
                                (True, False)  -> defaultTimeToJump


          newCanJump = if newTimeToJump < 0.0 
                          then False 
                          else if newTimeToJump == defaultTimeToJump
                                  then True
                                  else oldCanJump



fall :: World -> Character -> Character
fall World { level = Level { tiles = t } }
        c@Character { radius = r
                    , falling = isFalling
                    , currentVelocity = (dx, dy) } = 
    c { currentVelocity = (dx, dy + characterGravity) } 

characterGravity = 0.3

updateGraphics :: DeltaTime -> Character -> Character
updateGraphics dt c@Character { moveVelocity = maxVel
                              , falling      = isFalling
                              , currentVelocity = (vx, _)
                              , characterSpriteSheet = s } = 
    c { characterSpriteSheet = 
            let updated = updateSpriteSheet dt (updateState newState s)
                newState = case (signum vx, isFalling) of 
                                (1,  False) -> "RunRight"
                                (0,  False) -> "Stand"
                                (-1, False) -> "RunLeft" 
                                (1,  True) -> "FallRight"
                                (0,  True) -> "FallRight"
                                (-1, True) -> "FallLeft"
             in updateCurrentSprite (getCurrentSprite updated) { frameChangeTime = 0.1 + maxVel - abs vx } updated }

updateCharacter :: DeltaTime -> World -> Character -> Maybe Character
updateCharacter dt world ch = Just 
                           . updateGraphics dt
                           . updateCollisions dt world
                           . updatePosition dt
                           . fall world
                           -- . activate world
                           -- . attack world
                           . jump 
                           . move 
                           . applyBot BotMemory world 
                                      $ ch

applyBot :: BotMemory -> World -> Character -> Character
applyBot m w 
    c@Character { characterController = con@Controller { bot = maybeBot } } =
        case maybeBot of 
            Just b -> compose (map applyIntent (b m w c)) c
            Nothing -> c

instance Drawable Character where
    render screen camera renderer character = do
        render screen camera renderer spriteSheet
            where spriteSheet = (characterSpriteSheet character) { spriteSheetPosition = pos }
                  pos = currentPosition character `pointPlus` (- radius character,
                                                               - radius character)

