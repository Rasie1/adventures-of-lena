module World where

import Common
import Level
import Drawable
import Data.Maybe
import Character
import Types
import Data.Array
import Bot

instance Drawable World where
    render s c r world = do render s c r (level world)
                            mapM_ (render s c r) (characters world)


updateWorld :: Double -> World -> World
updateWorld dt = updatePickups
               . updateCharacters dt

mkWorld :: Level -> SpriteSheet -> SpriteSheet -> World
mkWorld lvl characterSpriteSheet enemySpriteSheet = World 
    { level = lvl
    , characters = spawnCharacters lvl characterSpriteSheet enemySpriteSheet
    , money = 0
    }

spawnCharacters :: Level -> SpriteSheet -> SpriteSheet -> [Character]
spawnCharacters lvl characterSpriteSheet enemySpriteSheet = 
    mapMaybe tileToCharacter (assocs (tiles lvl))
    where tileToCharacter ((x, y), Player) = 
                Just ((player characterSpriteSheet) { currentPosition = (fromIntegral x, fromIntegral y) })
          tileToCharacter ((x, y), Enemy) = 
                Just ((enemy enemySpriteSheet) { currentPosition = (fromIntegral x, fromIntegral y) })
          tileToCharacter _ = Nothing

findPlayer :: World -> Character
findPlayer = head . characters

updatePickups :: World -> World
updatePickups w = let p = findPlayer w
                      t = tiles $ level w
                      toCoord (x, y) = (floor x, floor y)
                      pos = currentPosition p
                      tile = getTile t (toCoord pos) 
                   in case tile of 
                        Money -> addMoney w { level = removeTile (toCoord pos) (level w) }
                        _     -> w

moneyMultiplier :: Int
moneyMultiplier = 25

addMoney :: World -> World
addMoney w = w { money = money w + moneyMultiplier }

updateCharacters :: Double -> World -> World
updateCharacters dt w = w { characters = mapMaybe (updateCharacter dt w) (characters w) }


anyCharacter spriteSheet = Character 
    { moveVelocity = 3
    , radius       = 0.5
    , inertia      = 0.1
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

    , characterSpriteSheet = spriteSheet
    }

player = anyCharacter

enemy spriteSheet = (anyCharacter spriteSheet) { characterController = Controller { port = 1, actions = [], bot = Just simpleMoveBot } }
