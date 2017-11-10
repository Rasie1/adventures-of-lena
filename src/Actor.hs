{-# LANGUAGE ExistentialQuantification #-}
module Actor where

import Common

class Actor a where
    act :: DeltaTime -> a -> Maybe a

instance Actor a => Actor (Maybe a) where
    act dt (Just x) = Just (act dt x)
    act _ Nothing = Nothing
