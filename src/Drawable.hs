module Drawable where

import qualified SDL
import Types
    
class Drawable a where
    render :: Camera -> SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> a -> IO ()

instance Drawable a => Drawable (Maybe a) where
    render c r t (Just x) = render c r t x
    render _ _ _ Nothing  = return ()
