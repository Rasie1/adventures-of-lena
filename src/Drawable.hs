module Drawable where

import qualified SDL
import Types
    
class Drawable a where
    render :: ScreenSize -> Camera -> SDL.Renderer -> a -> IO ()

instance Drawable a => Drawable (Maybe a) where
    render s c r (Just x) = render s c r x
    render _ _ _ Nothing  = return ()
