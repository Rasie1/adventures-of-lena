module Level where

import Data.Array
import Common
import Camera
import Drawable
import Control.Monad
import qualified SDL
import Types

instance Drawable Level where
    render screen camera renderer lvl = do
        forM_ (assocs (tiles lvl)) $ \((i, j), tile) ->
          renderTile i j tile
        where
          (texture, ti) = levelTexture lvl
          tileWidth = (fromIntegral $ SDL.textureWidth ti) / 24
          unitSize = levelUnitSize lvl
          tileRect = mkRect 0 0 tileWidth tileWidth

          getTilesheetCoords :: (Num a) => Tile -> (a, a)
          getTilesheetCoords Grass = (0, 288)
          getTilesheetCoords _ = (432, 624)

          renderTile x y t
            = if shouldCull then return ()
                            else SDL.copy renderer texture src dst
              where src = Just $ floor <$> moveTo (getTilesheetCoords t) tileRect
                    dst = Just $ floor <$> dstRect
                    dstRect = applyCamera screen unitSize camera (moveTo dstCoords tileRect)
                    dstCoordX = fromIntegral x * unitSize
                    dstCoordY = fromIntegral y * unitSize
                    dstCoords = (dstCoordX, dstCoordY)
                    dstPosX = fst $ getRectPosition dstRect
                    dstPosY = snd $ getRectPosition dstRect
                    shouldCull = dstPosX + unitSize < 0 || dstPosX > fst screen
                              || dstPosY + unitSize < 0 || dstPosY > snd screen


loadLevel :: String -> (SDL.Texture, SDL.TextureInfo) -> Double -> Level
loadLevel s t unitSize = Level (array ((0, 0), (levelWidth, levelHeight)) arrayElements) t unitSize
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

