module World where

import Level
import Drawable
import Data.Maybe
import Character
import Types

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
spawnCharacters lvl = [player]

player = Character 
    { moveSpeed  = 100
    , radius     = 256
    , inertia    = 0.5
    , jumpHeight = 100

    , currentPosition = (0, 0)
    , currentSpeed    = (0, 0)

    , characterController = Controller { port = 0, actions = [] }

    , moving    = NotMoving
    , falling   = False
    , using     = False
    , attacking = False
    , jumping   = False
    }
