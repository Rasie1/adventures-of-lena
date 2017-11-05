module Drawable where

import qualified SDL
import Camera
    
class Drawable a where
    render :: a -> Camera -> SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> IO ()

