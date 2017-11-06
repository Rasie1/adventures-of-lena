module Actor where

import Common

class Actor a where
    act :: DeltaTime -> a -> Maybe a
    getPosition :: a -> Position
    setPosition :: Position -> a -> a
