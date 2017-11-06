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

import System.Clock

diffTime :: TimeSpec -> TimeSpec -> DeltaTime
diffTime end start = (* 1e-9) $ fromIntegral $ toNanoSecs end - toNanoSecs  start

-- resolution = (1920, 1080)
resolution = (800, 600)



main :: IO ()
main = withSDL $ withSDLImage $ do
  setHintQuality
  withWindow "Game" resolution $ \w ->
    withRenderer w $ \r -> do
      t <- loadTextureWithInfo r "./assets/tiles.png"
      levelString <- readFile "./assets/tiles.map"

      initialTime <- getTime Monotonic

      let initialGameState = mkGameState (World { level = loadLevel levelString }) initialTime
      let updateTime time state = return state { currentTime = time }
      let processFPS state = do time <- getTime Monotonic
                                if (diffTime time (lastFPSPrintTime state) > 1.0)
                                    then do putStrLn $ "FPS: " ++ show (framesSinceLastFPSPrint state)
                                            return state { framesSinceLastFPSPrint = 0
                                                           , lastFPSPrintTime = time }
                                    else return state { framesSinceLastFPSPrint = framesSinceLastFPSPrint state + 1 }



      let update x = do time <- getTime Monotonic
                        state <- handleInput x <$> SDL.pollEvents
                        processFPS state
                            >>= updateTime time
                            >>= updateGame (diffTime time (currentTime state))
                            >>= renderFrame r t

      runApp update initialGameState

      SDL.destroyTexture (fst t)


runApp :: (Monad m) => (GameState -> m GameState) -> GameState -> m ()
runApp update = repeatUntil update exiting

repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)

updateGame :: DeltaTime -> GameState -> IO GameState
updateGame dt state = return (state { cameraPosition = (fst (cameraPosition state) + 100.0 * dt,
                                                        snd (cameraPosition state) + 100.0 * dt) })

renderFrame :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> GameState -> IO (GameState)
renderFrame renderer texture gameState = do
  SDL.clear renderer
  render (world gameState) (cameraPosition gameState) renderer texture
  SDL.present renderer
  return gameState
