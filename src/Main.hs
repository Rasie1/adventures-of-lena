{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Level

import qualified SDL
import qualified Common as C

import Control.Monad.Loops    
import Data.Foldable          
import Data.Array

data Intent
  = Idle
  | Quit

data World = World
  { exiting :: Bool
  , level :: Level
  }

mkWorld :: Level -> World
mkWorld lvl = World
  { exiting = False
  , level = lvl
  }

main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Game" (800, 600) $ \w ->
    C.withRenderer w $ \r -> do
      t <- C.loadTextureWithInfo r "./assets/tiles.png"

      let doRender = renderWorld r t

      levelString <- readFile "./assets/tiles.map"

      _ <- iterateUntilM
        exiting
        (\x ->
          updateWorld x <$> SDL.pollEvents
          >>= \x' -> x' <$ doRender x'
        )
        (mkWorld (loadLevel levelString))

      SDL.destroyTexture (fst t)


updateWorld :: World -> [SDL.Event] -> World
updateWorld w
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

applyIntent :: Intent -> World -> World
applyIntent Idle        = idleWorld
applyIntent Quit        = quitWorld


idleWorld :: World -> World
idleWorld = id


quitWorld :: World -> World
quitWorld w = w { exiting = True }


renderWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> IO ()
renderWorld r t w = do
  SDL.clear r
  drawWorld r t w
  SDL.present r

drawWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> IO ()
drawWorld renderer (texture, ti) world = do
    forM_ (assocs . tiles . level $ world) $ \((i, j), tile) ->
      renderTile i j tile
    where
      tileWidth :: Double
      tileWidth = (fromIntegral $ SDL.textureWidth ti) / 16
      tileRect = C.mkRect 0 0 tileWidth tileWidth

      getTilesheetCoords :: (Num a) => Tile -> (a, a)
      getTilesheetCoords Sky = (0, 0)
      getTilesheetCoords Grass = (64, 0)

      renderTile x y t
        = SDL.copy renderer texture
            (Just $ floor <$> tileRect `moveTo` getTilesheetCoords t)
            (Just $ floor <$> tileRect `moveTo` (fromIntegral x * tileWidth, fromIntegral y * tileWidth))



moveTo :: SDL.Rectangle a -> (a, a) -> SDL.Rectangle a
moveTo (SDL.Rectangle _ d) (x, y) = SDL.Rectangle (C.mkPoint x y) d
