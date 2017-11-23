module Drawable where

import qualified SDL
import Types
    
class Drawable a where
    render :: ScreenSize -> Camera -> SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> a -> IO ()

instance Drawable a => Drawable (Maybe a) where
    render s c r t (Just x) = render s c r t x
    render _ _ _ _ Nothing  = return ()
