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

mkWorld :: Level -> SpriteSheet -> World
mkWorld lvl spriteSheet = World 
    { level = lvl
    , characters = spawnCharacters lvl spriteSheet
    , money = 0
    }

spawnCharacters :: Level -> SpriteSheet -> [Character]
spawnCharacters lvl spriteSheet = 
    mapMaybe (tileToCharacter spriteSheet) (assocs (tiles lvl))

findPlayer :: World -> Character
findPlayer = head . characters

updatePickups :: World -> World
updatePickups w = let p = findPlayer w
                      t = tiles $ level w
                      toCoord (x, y) = (floor x, floor y)
                      pos = currentPosition p
                      tile = t ! (toCoord pos) 
                   in case tile of 
                        Money -> addMoney w { level = removeTile (toCoord pos) (level w) }
                        _     -> w

moneyMultiplier :: Int
moneyMultiplier = 25

addMoney :: World -> World
addMoney w = w { money = money w + moneyMultiplier }

updateCharacters :: Double -> World -> World
updateCharacters dt w = w { characters = mapMaybe (updateCharacter dt w) (characters w) }

tileToCharacter spriteSheet ((x, y), Player) = 
        Just ((player spriteSheet) { currentPosition = (fromIntegral x, fromIntegral y) })
tileToCharacter spriteSheet ((x, y), Enemy) = 
        Just ((enemy spriteSheet) { currentPosition = (fromIntegral x, fromIntegral y) })
tileToCharacter _ _ = Nothing

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

    , characterSpriteSheet = spriteSheet
    }

player = anyCharacter

enemy spriteSheet = (anyCharacter spriteSheet) { characterController = Controller { port = 1, actions = [], bot = Just simpleMoveBot } }
