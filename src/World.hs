module World where

import Level
import Drawable
import Data.Maybe
import Character
import Types
import Data.Array

instance Drawable World where
    render c r t world = do render c r t (level world)
                            mapM_ (render c r t) (characters world)

updateWorld :: Double -> World -> Maybe World
updateWorld dt w = Just w { characters = mapMaybe (updateCharacter dt w) (characters w) }

mkWorld :: Level -> World
mkWorld lvl = World 
    { level = lvl
    , characters = spawnCharacters lvl
    }

spawnCharacters :: Level -> [Character]
spawnCharacters lvl = 
    mapMaybe tileToCharacter (assocs (tiles lvl))

tileToCharacter ((x, y), Player) = 
        Just (player { currentPosition = (fromIntegral x, fromIntegral y) })
tileToCharacter _ = Nothing

player = Character 
    { moveSpeed  = 3
    , radius     = 0.5
    , inertia    = 0.1
    , jumpHeight = 5

    , currentPosition = (0, 0)
    , currentSpeed    = (0, 0)

    , characterController = Controller { port = 0, actions = [] }

    , moving    = NotMoving
    , falling   = True
    , using     = False
    , attacking = False
    , jumping   = False
    }
