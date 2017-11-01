module Level where

import Control.Monad.Loops    
import Data.Foldable          
import Data.Array

data Level = Level
    { tiles :: Array (Int, Int) Tile
    } deriving Show


data Tile = Sky | Grass deriving Show


loadLevel :: String -> Level
loadLevel s = Level $ array ((0, 0), (len, len)) arrayElements
            where 
                  arrayElements :: [((Int, Int), Tile)]
                  arrayElements = foldl f [] numberedRows
                  
                  numberedTiles :: [[(Int, Tile)]]
                  numberedTiles = map (zip [0..] . map toTile) (lines s)
                  
                  numberedRows = zip [0..] numberedTiles
                  
                  f :: [((Int, Int), Tile)] -> (Int, [(Int, Tile)]) -> [((Int, Int), Tile)]
                  f acc (i, xs) = map (g i) xs ++ acc
                  
                  g i (j, tile) = ((j, i), tile)

                  len = length numberedTiles - 1

toTile :: Char -> Tile
toTile 'g' = Grass
toTile _ = Sky

