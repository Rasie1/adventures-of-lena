module Character where

import Data.Array
import Common
import Camera
import Drawable
import Actor
import Level
import Control.Monad
import qualified SDL

data Character = Character
    { characterPosition :: Position
    , characterSpeed :: Speed
    }

applySpeed :: DeltaTime -> Position -> Speed -> Position
applySpeed dt (posx, posy) (spdx, spdy) = (posx + spdx * dt, posy + spdy * dt)

instance Actor Character where
    act dt ch = Just ch { characterPosition = applySpeed dt (characterPosition ch) (characterSpeed ch) }

instance Drawable Character where
    render character camera renderer (texture, ti) = do
        renderTile 0 (-1) Sky camera
        where
          tileWidth :: Double
          tileWidth = (fromIntegral $ SDL.textureWidth ti) / 16
          tileRect = mkRect 0 0 tileWidth tileWidth

          getTilesheetCoords :: (Num a) => Tile -> (a, a)
          getTilesheetCoords Sky = (0, 0)
          getTilesheetCoords Grass = (64, 0)

          renderTile x y t camera
            = SDL.copy renderer texture
                (Just $ floor <$> moveTo (getTilesheetCoords t) tileRect)
                (Just $ floor <$> applyCamera camera (moveTo (fromIntegral x * tileWidth, fromIntegral y * tileWidth) tileRect))
