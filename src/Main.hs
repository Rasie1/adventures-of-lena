{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Level
import World
import GameState
import Input
import Drawable

import qualified SDL
import Common

import Control.Monad
import Data.Foldable          
import Data.Array

import System.Clock

diffTime :: TimeSpec -> TimeSpec -> DeltaTime
diffTime end start = (* 1e-9) $ fromIntegral $ toNanoSecs end - toNanoSecs  start

main :: IO ()
main = withSDL $ withSDLImage $ do
  setHintQuality
  withWindow "Game" (800, 600) $ \w ->
    withRenderer w $ \r -> do
      t <- loadTextureWithInfo r "./assets/tiles.png"
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
  render (world gameState) (cameraPosition gameState) renderer texture
  SDL.present renderer
