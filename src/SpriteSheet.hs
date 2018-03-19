module SpriteSheet where

import Common
import qualified SDL
import Types
import Camera
import Drawable
import Data.Map
import Data.Text
-- import Control.Lens

-- makeLenses ''SpriteSheet
-- makeLenses ''SpriteInfo

mkStaticSprite :: (Int, Int) -> (Int, Int) -> Double -> (SDL.Texture, SDL.TextureInfo) -> Position -> SpriteSheet
mkStaticSprite frameCoords' frameSize' unitSize' texture pos = SpriteSheet 
  { sprites = fromList [("", SpriteInfo 
    { framesCount  = 1
    , currentFrame = 0
    , frameCoords  = frameCoords'
    , frameSize    = frameSize'
    , unitSize     = unitSize'
    , gapBetweenFrames  = 0
    , frameChangeTime   = 0
    , timeSinceChange   = 0
    })]
  , currentSprite = ""
  , spriteSheetTexture  = texture
  , spriteSheetPosition = pos
  }

getCurrentSprite :: SpriteSheet -> SpriteInfo
getCurrentSprite spriteSheet = (sprites spriteSheet) ! (currentSprite spriteSheet)

updateCurrentSprite :: SpriteInfo -> SpriteSheet -> SpriteSheet
updateCurrentSprite sprite sheet = sheet { sprites = adjust (\_ -> sprite) 
                                                            (currentSprite sheet) 
                                                            (sprites sheet) }    



updateSprite :: DeltaTime -> SpriteInfo -> SpriteInfo
updateSprite deltaTime sprite = let newTime = timeSinceChange sprite + deltaTime
                                 in if newTime > frameChangeTime sprite 
                                       then sprite { timeSinceChange = 0
                                                   , currentFrame = mod (currentFrame sprite + 1) (framesCount sprite) }
                                       else sprite { timeSinceChange = newTime }


updateSpriteSheet :: DeltaTime -> SpriteSheet -> SpriteSheet
updateSpriteSheet deltaTime spriteSheet = 
            spriteSheet { sprites = adjust (updateSprite deltaTime)
                                           (currentSprite spriteSheet) 
                                           (sprites spriteSheet) }
                                 
updateState :: Text -> SpriteSheet -> SpriteSheet
updateState key s = s { currentSprite = key }


instance Drawable SpriteSheet where
    render screen camera renderer spriteSheet = do
        renderSpriteSheet (spriteSheetPosition spriteSheet) 
        where
          sprite = getCurrentSprite spriteSheet
          (texture, ti) = spriteSheetTexture spriteSheet
          tileWidth  = fromIntegral . fst $ frameSize sprite
          tileHeight = fromIntegral . snd $ frameSize sprite
          tileBeginX  = fromIntegral . fst $ frameCoords sprite
          tileBeginY = fromIntegral . snd $ frameCoords sprite
          tileRect = mkRect tileBeginX tileBeginY tileWidth tileHeight

          renderSpriteSheet (x, y)
            = SDL.copy renderer texture src dst
              where src = Just $ floor <$> moveBy (tileWidth * fromIntegral (currentFrame sprite), 0.0) tileRect
                    dst = Just $ floor <$> applyCamera screen (unitSize sprite) camera (moveTo dstPos tileRect)
                    dstPosX = x * (unitSize sprite)
                    dstPosY = y * (unitSize sprite)
                    dstPos = (dstPosX, dstPosY)
