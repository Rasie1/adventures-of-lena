module Drawable where

import qualified SDL
import Camera
import Actor
    
class Actor a => Drawable a where
    render :: a -> Camera -> SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> IO ()

instance Drawable a => Drawable (Maybe a) where
    render (Just x) c r t = render x c r t
    render Nothing _ _ _ = return ()
