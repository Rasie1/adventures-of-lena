{-# LANGUAGE ExistentialQuantification #-}
module World where

import Level
import Actor
import Drawable
import Data.Maybe
import Character

data World = World
  { level :: Level
  , characters :: [Character]
  }

instance Drawable World where
    render world = do render (level world)
                      -- render <$> actors

instance Actor World where
    act dt w = Just w { characters = mapMaybe (act dt) (characters w) }

mkWorld :: Level -> World
mkWorld lvl = World 
    { level = lvl
    , characters = spawnCharacters lvl
    }

spawnCharacters :: Level -> [Character]
spawnCharacters lvl = [Character { characterSpeed = (0,0) }]
