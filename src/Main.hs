{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Level

import qualified SDL
import qualified Common as C

import Control.Monad
import Control.Monad.Loops    
import Data.Foldable          
import Data.Array

import System.Clock
import Control.Exception
import Formatting
import Formatting.Clock

data Intent
  = Idle
  | Quit

data GameState = GameState
  { exiting :: Bool
  , level :: Level
  , currentTime :: TimeSpec
  }

mkGameState :: Level -> TimeSpec -> GameState
mkGameState lvl time = GameState
  { exiting = False
  , level = lvl
  , currentTime = time
  }

data Camera = Camera (Double, Double)

type DeltaTime = Double

diffTime :: TimeSpec -> TimeSpec -> DeltaTime
diffTime end start = (* 1e-9) $ fromIntegral $ toNanoSecs end - toNanoSecs  start

main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Game" (800, 600) $ \w ->
    C.withRenderer w $ \r -> do
      t <- C.loadTextureWithInfo r "./assets/tiles.png"
      levelString <- readFile "./assets/tiles.map"

      currentTime <- getTime Monotonic

      let doRender = renderGameState r t
      let initialGameState = mkGameState (loadLevel levelString) currentTime
      let update x = updateGameState x <$> SDL.pollEvents
                     >>= \x' -> x' <$ doRender x'

      runApp update initialGameState

      SDL.destroyTexture (fst t)


runApp :: (Monad m) => (GameState -> m GameState) -> GameState -> m ()
runApp update = repeatUntil update exiting

repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)

updateGameState :: GameState -> [SDL.Event] -> GameState
updateGameState w
  = foldl' (flip applyIntent) w
  . fmap (payloadToIntent . SDL.eventPayload)

payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit
payloadToIntent (SDL.MouseMotionEvent e) = motionIntent e
payloadToIntent (SDL.MouseButtonEvent e) = buttonIntent e
payloadToIntent _                        = Idle

motionIntent :: SDL.MouseMotionEventData -> Intent
motionIntent _ = Idle
-- motionIntent e = Hover q
--   where
--     q = selectQuadrant x y
--     (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e


  -- | SDL.mouseButtonEventMotion e == SDL.Pressed -> Down
  --
buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent _ = Idle
-- buttonIntent e = t q
--   where
--     q = selectQuadrant x y
--     (SDL.P (SDL.V2 x y)) = SDL.mouseButtonEventPos e
--     t = if SDL.mouseButtonEventMotion e == SDL.Pressed
--            then Press
--            else Release

applyIntent :: Intent -> GameState -> GameState
applyIntent Idle        = idleGameState
applyIntent Quit        = quitGameState


idleGameState :: GameState -> GameState
idleGameState = id


quitGameState :: GameState -> GameState
quitGameState w = w { exiting = True }


renderGameState :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> GameState -> IO ()
renderGameState r t w = do
  SDL.clear r
  drawGameState r t w (Camera (0, 0))
  SDL.present r

drawGameState :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> GameState -> Camera -> IO ()
drawGameState renderer (texture, ti) world camera = do
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
            (Just $ floor <$> tileRect `moveTo` getTilesheetCoords t)
            (Just $ floor <$> tileRect `moveTo` (fromIntegral x * tileWidth, fromIntegral y * tileWidth))

applyCamera :: Camera -> SDL.Rectangle a -> SDL.Rectangle a
applyCamera c r = r

moveTo :: SDL.Rectangle a -> (a, a) -> SDL.Rectangle a
moveTo (SDL.Rectangle _ d) (x, y) = SDL.Rectangle (C.mkPoint x y) d
