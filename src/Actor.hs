module Actor where

import Common

class Actor a where
    act :: DeltaTime -> a -> Maybe a

instance Actor a => Actor (Maybe a) where
    act dt w (Just x) = Just (act dt w x)
    act _ _ Nothing = Nothing
