module Level where

import Data.Array
import Common
import Camera
import Drawable
import Control.Monad
import qualified SDL
import Types

instance Drawable Level where
    render camera renderer (texture, ti) lvl = do
        forM_ (assocs (tiles lvl)) $ \((i, j), tile) ->
          renderTile i j tile camera
        where
          tileWidth :: Double
          tileWidth = (fromIntegral $ SDL.textureWidth ti) / 24
          tileRect = mkRect 0 0 tileWidth tileWidth

          getTilesheetCoords :: (Num a) => Tile -> (a, a)
          getTilesheetCoords Sky = (288, 416)
          getTilesheetCoords Grass = (0, 192)

          renderTile x y t camera
            = SDL.copy renderer texture
                (Just $ floor <$> moveTo (getTilesheetCoords t) tileRect)
                (Just $ floor <$> applyCamera camera (moveTo (fromIntegral x * tileWidth, fromIntegral y * tileWidth) tileRect))


loadLevel :: String -> Level
loadLevel s = Level $ array ((0, 0), (levelWidth, levelHeight)) arrayElements
            where 
                  arrayElements :: [((Int, Int), Tile)]
                  arrayElements = foldl f [] numberedRows
                  
                  levelWidth = (length . head . lines) s - 1
                  levelHeight = length numberedRows - 1
                  
                  numberedTiles :: [[(Int, Tile)]]
                  numberedTiles = map (zip [0..] . map toTile) (lines s)
                  
                  numberedRows = zip [0..] numberedTiles
                  
                  f :: [((Int, Int), Tile)] -> (Int, [(Int, Tile)]) -> [((Int, Int), Tile)]
                  f acc (i, xs) = map (g i) xs ++ acc
                  
                  g i (j, tile) = ((j, i), tile)

toTile :: Char -> Tile
toTile 'g' = Grass
toTile 'p' = Player
toTile 'e' = Enemy
toTile _ = Sky

isSolid :: Tile -> Bool
isSolid Sky = False
isSolid Grass = True
isSolid Player = False
isSolid Enemy = False

