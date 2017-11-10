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

