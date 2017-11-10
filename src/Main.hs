{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Level
import World
import GameState
import Input
import Drawable
import Actor
import Character

import qualified SDL

import qualified SDL.Image

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text              (Text)
import Data.Maybe

import SDL (($=))
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

      let initialGameState = mkGameState (World { level = loadLevel levelString
                                                , characters = [Character { characterSpeed = (0,0) }] }) initialTime
      let updateTime time state = return state { currentTime = time }
      let processFPS time state = if diffTime time (lastFPSPrintTime state) > 1.0
                                     then do putStrLn $ "FPS: " ++ show (framesSinceLastFPSPrint state)
                                             return state { framesSinceLastFPSPrint = 0
                                                          , lastFPSPrintTime = time }
                                     else return state { framesSinceLastFPSPrint = framesSinceLastFPSPrint state + 1 }



      let update x = do time <- getTime Monotonic
                        state <- handleInput x <$> SDL.pollEvents
                        processFPS time state
                            >>= updateTime time
                            >>= updateGame (diffTime time (currentTime state))
                            >>= renderFrame r t

      runApp update initialGameState

      SDL.destroyTexture (fst t)


runApp :: (Monad m) => (GameState -> m GameState) -> GameState -> m ()
runApp update = repeatUntil update (isNothing . world)

repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)

updateGame :: DeltaTime -> GameState -> IO GameState
updateGame dt state = return (state { world = updateWorld (world state) })
                        where updateWorld Nothing = Nothing
                              updateWorld (Just w) = act dt w

renderFrame :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> GameState -> IO (GameState)
renderFrame renderer texture gameState = do
  SDL.clear renderer
  render (world gameState) (cameraPosition gameState) renderer texture
  SDL.present renderer
  return gameState

withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initialize []
  void op
  SDL.quit

withSDLImage :: (MonadIO m) => m a -> m ()
withSDLImage op = do
  SDL.Image.initialize []
  void op
  SDL.Image.quit


withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
  w <- SDL.createWindow title p
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w

    where
      p = SDL.defaultWindow { SDL.windowInitialSize = z }
      z = SDL.V2 (fromIntegral x) (fromIntegral y)


withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
  r <- SDL.createRenderer w (-1) rendererConfig
  void $ op r
  SDL.destroyRenderer r


rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False
  }


renderSurfaceToWindow :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
renderSurfaceToWindow w s i
  = SDL.surfaceBlit i Nothing s Nothing
  >> SDL.updateWindowSurface w


setHintQuality :: (MonadIO m) => m ()
setHintQuality = SDL.HintRenderScaleQuality $= SDL.ScaleNearest


loadTextureWithInfo :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTextureWithInfo r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure (t, i)
