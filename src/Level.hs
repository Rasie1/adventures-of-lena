module Level where

import Data.Array
import Common
import Camera
import Drawable
import Control.Monad
import qualified SDL

data Level = Level
    { tiles :: Array (Int, Int) Tile
    } deriving Show

instance Drawable Level where
    render lvl camera renderer (texture, ti) = do
        forM_ (assocs (tiles lvl)) $ \((i, j), tile) ->
          renderTile i j tile camera
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


data Tile = Sky | Grass deriving Show


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
toTile _ = Sky

