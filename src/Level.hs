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
        renderBackground
        forM_ (assocs (tiles lvl)) $ \((i, j), tile) ->
          renderTile i j tile
        where
          (texture, ti) = levelTexture lvl
          (btexture, bti) = backgroundTexture lvl
          tileWidth = (fromIntegral $ SDL.textureWidth ti) / 24
          unitSize = levelUnitSize lvl
          tileRect = mkRect 0 0 tileWidth tileWidth

          getTilesheetCoords :: (Num a) => Tile -> (a, a)
          getTilesheetCoords GroundThin1 = (0, 288)
          getTilesheetCoords GroundThin2 = (96, 288)
          getTilesheetCoords GroundTop1 = (48, 48)
          getTilesheetCoords GroundTop2 = (96, 48)
          getTilesheetCoords GroundTopLeft = (0, 48)
          getTilesheetCoords GroundTopRight = (144, 48)
          getTilesheetCoords GroundCenter1 = (48, 96)
          getTilesheetCoords GroundCenter2 = (96, 96)
          getTilesheetCoords GroundLeft = (0, 96)
          getTilesheetCoords GroundRight = (144, 96)
          getTilesheetCoords GroundBottomLeft = (0, 144)
          getTilesheetCoords GroundBottomRight = (144, 144)
          getTilesheetCoords GroundBottom1 = (48, 144)
          getTilesheetCoords GroundBottom2 = (96, 144)
          getTilesheetCoords GroundGrassLeft = (240, 144)
          getTilesheetCoords GroundGrassRight = (192, 144)
          getTilesheetCoords GroundLeftBorder = (192, 192)
          getTilesheetCoords GroundRightBorder = (240, 192)
          getTilesheetCoords Spikes1 = (1056, 0)
          getTilesheetCoords Spikes2 = (1056, 48)

          getTilesheetCoords Money = (960, 96)
          getTilesheetCoords _ = (432, 624)

          renderBackground = SDL.copy renderer 
                                      btexture 
                                      (Just (mkRect 0 0 (SDL.textureWidth bti) 
                                                        (SDL.textureHeight bti)))
                                      (Just (mkRect 0 0 1280 960))

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


loadLevel :: String -> (SDL.Texture, SDL.TextureInfo) -> (SDL.Texture, SDL.TextureInfo) -> Double -> Level
loadLevel s t bt unitSize = Level (array ((0, 0), (levelWidth, levelHeight)) arrayElements) t bt unitSize
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

removeTile :: (Int, Int) -> Level -> Level
removeTile pos lvl = lvl { tiles = tiles lvl // [(pos, Sky)] }

getTile :: Array (Int, Int) Tile -> (Int, Int) -> Tile
getTile a k = if inRange (bounds a) k then a ! k
                                      else Sky

toTile :: Char -> Tile
toTile 'w' = GroundTop1
toTile 'e' = GroundTop2
toTile 'g' = GroundThin1
toTile 'b' = GroundThin2
toTile 'q' = GroundTopLeft
toTile 'r' = GroundTopRight
toTile 's' = GroundCenter1
toTile 'd' = GroundCenter2
toTile 'a' = GroundLeft
toTile 'f' = GroundRight
toTile 'z' = GroundBottomLeft
toTile 'v' = GroundBottomRight
toTile 'x' = GroundBottom1
toTile 'c' = GroundBottom2
toTile '0' = Player
toTile '9' = Enemy
toTile '`' = Money
toTile '1' = RedDye
toTile '2' = BlueDye
toTile '3' = GreenDye
toTile 'h' = Spikes1
toTile 'n' = Spikes2
toTile 'm' = GroundGrassLeft
toTile '/' = GroundGrassRight
toTile ',' = GroundLeftBorder
toTile '.' = GroundRightBorder
toTile '<' = KillZone
toTile _ = Sky

isSolid :: Tile -> Bool
isSolid GroundTop1 = True
isSolid GroundTop2 = True
isSolid GroundThin1 = True
isSolid GroundThin2 = True
isSolid GroundTopLeft = True
isSolid GroundTopRight = True
isSolid GroundCenter1 = True
isSolid GroundCenter2 = True
isSolid GroundLeft = True
isSolid GroundRight = True
isSolid GroundBottomLeft = True
isSolid GroundBottomRight = True
isSolid GroundBottom1 = True
isSolid GroundBottom2 = True
isSolid GroundGrassLeft = True
isSolid GroundGrassRight = True
isSolid GroundLeftBorder = True
isSolid GroundRightBorder = True
isSolid _ = False

isDeadly :: Tile -> Bool
isDeadly Spikes2  = True
isDeadly KillZone = True
isDeadly _ = False
