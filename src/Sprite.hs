module Sprite where

import Common
import qualified SDL
import Types
import Camera
import Drawable

mkSprite :: Int -> 
           (Int, Int) -> (Int, Int) -> 
           Int -> Double -> 
           (SDL.Texture, SDL.TextureInfo) ->
           Position -> Sprite
mkSprite framesCount' frameCoords' frameSize' gapBetweenFrames' frameChangeTime' texture  pos = Sprite
  { framesCount  = framesCount'
  , currentFrame = 0
  , frameCoords  = frameCoords'
  , frameSize    = frameSize'
  , gapBetweenFrames  = gapBetweenFrames'
  , frameChangeTime   = frameChangeTime'
  , spriteTexture     = texture
  , spritePosition = pos
  }

mkStaticSprite :: (Int, Int) -> (Int, Int) -> (SDL.Texture, SDL.TextureInfo) -> Position -> Sprite
mkStaticSprite frameCoords' frameSize' texture pos = Sprite 
  { framesCount  = 1
  , currentFrame = 0
  , frameCoords  = frameCoords'
  , frameSize    = frameSize'
  , gapBetweenFrames  = 0
  , frameChangeTime   = 0
  , spriteTexture     = texture
  , spritePosition    = pos
  }

instance Drawable Sprite where
    render screen camera renderer sprite = do
        renderSprite (spritePosition sprite) 
        where
          (texture, ti) = spriteTexture sprite
          tileWidth = (fromIntegral $ SDL.textureWidth ti) / 24
          tileRect = mkRect 0 0 tileWidth tileWidth
          getTilesheetCoords :: (Num a) => (a, a)
          getTilesheetCoords = (192, 192)

          renderSprite (x, y)
            = SDL.copy renderer texture src dst
              where src = Just $ floor <$> moveTo getTilesheetCoords tileRect
                    dst = Just $ floor <$> applyCamera screen tileWidth camera (moveTo dstPos tileRect)
                    dstPosX = x * tileWidth
                    dstPosY = y * tileWidth
                    dstPos = (dstPosX, dstPosY)
