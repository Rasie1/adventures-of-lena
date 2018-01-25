module World where

import Level
import Drawable
import Data.Maybe
import Character
import Types
import Data.Array
import Bot

instance Drawable World where
    render s c r t world = do render s c r t (level world)
                              mapM_ (render s c r t) (characters world)

updateWorld :: Double -> World -> World
updateWorld dt w = w { characters = mapMaybe (updateCharacter dt w) (characters w) }

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
tileToCharacter ((x, y), Enemy) = 
        Just (enemy { currentPosition = (fromIntegral x, fromIntegral y) })
tileToCharacter _ = Nothing

anyCharacter = Character { moveVelocity = 3
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
    }

player = anyCharacter

enemy = anyCharacter { characterController = Controller { port = 1, actions = [], bot = Just simpleMoveBot } }
