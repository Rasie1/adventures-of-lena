module Sprite where

import Common
import qualified SDL
import Types
import Camera
import Drawable

mkSprite :: Int -> (Int, Int) -> (Int, Int) ->
           Double -> Int -> Double -> 
           (SDL.Texture, SDL.TextureInfo) ->
           Position -> Sprite
mkSprite framesCount' frameCoords' frameSize' unitSize' gapBetweenFrames' frameChangeTime' texture  pos = Sprite
  { framesCount  = framesCount'
  , currentFrame = 0
  , frameCoords  = frameCoords'
  , frameSize    = frameSize'
  , unitSize     = unitSize'
  , gapBetweenFrames  = gapBetweenFrames'
  , frameChangeTime   = frameChangeTime'
  , timeSinceChange   = 0
  , spriteTexture     = texture
  , spritePosition = pos
  }

mkStaticSprite :: (Int, Int) -> (Int, Int) -> Double -> (SDL.Texture, SDL.TextureInfo) -> Position -> Sprite
mkStaticSprite frameCoords' frameSize' unitSize' texture pos = Sprite 
  { framesCount  = 1
  , currentFrame = 0
  , frameCoords  = frameCoords'
  , frameSize    = frameSize'
  , unitSize     = unitSize'
  , gapBetweenFrames  = 0
  , frameChangeTime   = 0
  , timeSinceChange   = 0
  , spriteTexture     = texture
  , spritePosition    = pos
  }

updateSprite :: DeltaTime -> Sprite -> Sprite
updateSprite deltaTime sprite = let newTime = timeSinceChange sprite + deltaTime
                                 in if newTime > frameChangeTime sprite 
                                       then sprite { timeSinceChange = 0
                                                   , currentFrame = mod (currentFrame sprite + 1) (framesCount sprite) }
                                       else sprite { timeSinceChange = newTime }

instance Drawable Sprite where
    render screen camera renderer sprite = do
        renderSprite (spritePosition sprite) 
        where
          (texture, ti) = spriteTexture sprite
          tileWidth  = fromIntegral . fst $ frameSize sprite
          tileHeight = fromIntegral . snd $ frameSize sprite
          tileBeginX  = fromIntegral . fst $ frameCoords sprite
          tileBeginY = fromIntegral . snd $ frameCoords sprite
          tileRect = mkRect tileBeginX tileBeginY tileWidth tileHeight

          renderSprite (x, y)
            = SDL.copy renderer texture src dst
              where src = Just $ floor <$> moveBy (tileWidth * fromIntegral (currentFrame sprite), 0.0) tileRect
                    dst = Just $ floor <$> applyCamera screen (unitSize sprite) camera (moveTo dstPos tileRect)
                    dstPosX = x * (unitSize sprite)
                    dstPosY = y * (unitSize sprite)
                    dstPos = (dstPosX, dstPosY)
