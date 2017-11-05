{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Level
import World
import GameState
import Input

import qualified SDL
import qualified Common as C

import Control.Monad
import Data.Foldable          
import Data.Array

import System.Clock

diffTime :: TimeSpec -> TimeSpec -> DeltaTime
diffTime end start = (* 1e-9) $ fromIntegral $ toNanoSecs end - toNanoSecs  start

main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Game" (800, 600) $ \w ->
    C.withRenderer w $ \r -> do
      t <- C.loadTextureWithInfo r "./assets/tiles.png"
      levelString <- readFile "./assets/tiles.map"

      initialTime <- getTime Monotonic

      let initialGameState = mkGameState (World { level = loadLevel levelString }) initialTime
      let update x = do inputHandled <- handleInput x <$> SDL.pollEvents
                        currentTime <- getTime Monotonic
                        updated <- updateGame inputHandled
                        renderFrame r t updated
                        return updated

      runApp update initialGameState

      SDL.destroyTexture (fst t)


runApp :: (Monad m) => (GameState -> m GameState) -> GameState -> m ()
runApp update = repeatUntil update exiting

repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)

updateGame :: GameState -> IO GameState
updateGame state = return (state { cameraPosition = (fst (cameraPosition state) + 1.0, snd (cameraPosition state) + 1.0) })

renderFrame :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> GameState -> IO ()
renderFrame renderer texture gameState = do
  SDL.clear renderer
  drawWorld renderer texture (world gameState) (cameraPosition gameState)
  SDL.present renderer

drawWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> Camera -> IO ()
drawWorld renderer (texture, ti) world camera = do
    forM_ (assocs . tiles . level $ world) $ \((i, j), tile) ->
      renderTile i j tile camera
    where
      tileWidth :: Double
      tileWidth = (fromIntegral $ SDL.textureWidth ti) / 16
      tileRect = C.mkRect 0 0 tileWidth tileWidth

      getTilesheetCoords :: (Num a) => Tile -> (a, a)
      getTilesheetCoords Sky = (0, 0)
      getTilesheetCoords Grass = (64, 0)

      renderTile x y t camera
        = SDL.copy renderer texture
            (Just $ floor <$> moveTo (getTilesheetCoords t) tileRect)
            (Just $ floor <$> applyCamera camera (moveTo (fromIntegral x * tileWidth, fromIntegral y * tileWidth) tileRect))


applyCamera :: Camera -> SDL.Rectangle Double -> SDL.Rectangle Double
applyCamera (x, y) = moveBy (x, y)

moveBy :: (Num a) => (a, a) -> SDL.Rectangle a -> SDL.Rectangle a
moveBy (dx, dy) (SDL.Rectangle (SDL.P (SDL.V2 x y)) d) = SDL.Rectangle (C.mkPoint (x + dx) (y + dy)) d

moveTo :: (a, a) -> SDL.Rectangle a -> SDL.Rectangle a
moveTo (x, y) (SDL.Rectangle _ d) = SDL.Rectangle (C.mkPoint x y) d
