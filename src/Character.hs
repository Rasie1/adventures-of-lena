module Character where

import Common
import Camera
import Drawable
import Types
import Level
import Bot
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
applyIntent Jump      c = c { jumping = canJump c }
applyIntent Attack    c = c { attacking = canAttack c && moving c == NotMoving
                            , jumping = flyMode c
                            , canJump = flyMode c }
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

move :: DeltaTime -> Character -> Character
move dt c@Character  { moving = moving 
                     , currentVelocity = (dx, dy)
                     , inertia = i
                     , falling = falling
                     , airInertia = ai
                     , moveVelocity = m } = 
    case moving of
                MovingLeft  -> c { currentVelocity = (-m, dy), lastDirection = Types.Left }
                MovingRight -> c { currentVelocity = ( m, dy), lastDirection = Types.Right }
                NotMoving   -> c { currentVelocity = (dx * if falling then ai else i, dy) }


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



fall :: DeltaTime -> World -> Character -> Character
fall dt World { level = Level { tiles = t } }
        c@Character { radius = r
                    , falling = isFalling
                    , currentVelocity = (dx, dy) } = 
    c { currentVelocity = (dx, dy + characterGravity * dt) } 

characterGravity = 0.3 / 0.0166

updateGraphics :: DeltaTime -> Character -> Character
updateGraphics dt c@Character { moveVelocity = maxVel
                              , falling      = falling
                              , attacking    = attacking
                              , timeSinceAttack = timeSinceAttack
                              , lastDirection   = lastDirection
                              , currentVelocity = (vx, _)
                              , characterSpriteSheet = s } = 
    c { characterSpriteSheet = 
            let updated = updateSpriteSheet dt (updateState newState s)
                newState = case (signum (round vx), falling, timeSinceAttack < 0.3, lastDirection) of 
                                (_, _, True, Types.Left) -> "AttackLeft"
                                (_, _, True, Types.Right) -> "AttackRight"
                                (1,  False, False, _) -> "RunRight"
                                (-1, False, False, _) -> "RunLeft" 
                                (0,  False, False, Types.Left) -> "StandLeft"
                                (0,  False, False, Types.Right) -> "StandRight"
                                (1,  True, False, _) -> "FallRight"
                                (-1, True, False, _) -> "FallLeft"
                                (0,  True, False, Types.Left) -> "FallLeft"
                                (0,  True, False, Types.Right ) -> "FallRight"
             in updateCurrentSprite (getCurrentSprite updated) { frameChangeTime = if timeSinceAttack >= 0.3 then 0.1 + maxVel - abs vx else 0.1 } updated }


attack :: DeltaTime -> Character -> Character
attack dt c = c { timeSinceAttack = if attacking c then 0 
                                                   else timeSinceAttack c + dt }

updateCharacter :: DeltaTime -> World -> Character -> Maybe Character
updateCharacter dt world ch = Just 
                           . updateGraphics dt
                           . updateCollisions dt world
                           . updatePosition dt
                           . fall dt world
                           -- . activate world
                           . attack dt
                           . jump 
                           . move dt
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

mkCharacter :: SpriteSheet -> Character
mkCharacter spriteSheet = Character 
    { moveVelocity = 3
    , radius       = 0.5
    , inertia      = 0.75
    , airInertia   = 0.99
    , jumpPower    = 1

    , currentPosition = (0, 0)
    , currentVelocity = (0, 0)

    , characterController = Controller { port = 0, actions = [], bot = Nothing }

    , moving    = NotMoving
    , falling   = True
    , using     = False
    , attacking = False

    , jumping   = False
    , canJump    = True
    , timeToJump = 1.0
    , lastDirection = Types.Right
    , canAttack = False
    , timeSinceAttack = 999.0
    , flyMode = False

    , characterSpriteSheet = spriteSheet
    }

enemy :: SpriteSheet -> Character
enemy spriteSheet = (mkCharacter spriteSheet) { characterController = Controller { port = 1, actions = [], bot = Just simpleMoveBot } }
