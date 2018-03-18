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

mkWorld :: Level -> Sprite -> World
mkWorld lvl sprite = World 
    { level = lvl
    , characters = spawnCharacters lvl sprite
    }

spawnCharacters :: Level -> Sprite -> [Character]
spawnCharacters lvl sprite = 
    mapMaybe (tileToCharacter sprite) (assocs (tiles lvl))

tileToCharacter sprite ((x, y), Player) = 
        Just ((player sprite) { currentPosition = (fromIntegral x, fromIntegral y) })
tileToCharacter sprite ((x, y), Enemy) = 
        Just ((enemy sprite) { currentPosition = (fromIntegral x, fromIntegral y) })
tileToCharacter _ _ = Nothing

anyCharacter sprite = Character 
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

    , characterSprite = sprite
    }

player = anyCharacter

enemy sprite = (anyCharacter sprite) { characterController = Controller { port = 1, actions = [], bot = Just simpleMoveBot } }
