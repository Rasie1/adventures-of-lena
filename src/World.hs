module World where

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
updateWorld dt w = w { characters = mapMaybe (updateCharacter dt w) (characters w) }

mkWorld :: Level -> SpriteSheet -> World
mkWorld lvl spriteSheet = World 
    { level = lvl
    , characters = spawnCharacters lvl spriteSheet
    }

spawnCharacters :: Level -> SpriteSheet -> [Character]
spawnCharacters lvl spriteSheet = 
    mapMaybe (tileToCharacter spriteSheet) (assocs (tiles lvl))

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
