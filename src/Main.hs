{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import qualified Common as C

import Control.Monad.Loops    
import Data.Foldable          


data Intent
  = Idle
  | Quit


data World = World
  { exiting :: Bool
  , tilemap :: Tilemap
  }

data Tilemap = Tilemap
    { tiles :: [[(Int, Int, Tile)]]
    }

data Tile = Sky | Grass

initialWorld :: World
initialWorld = World
  { exiting = False
  , tilemap = initialTilemap
  }

initialTilemap :: Tilemap
initialTilemap = Tilemap
    { tiles =  [[(0, 0, Sky), (1, 0, Sky), (2, 0, Grass)] 
               ,[(0, 1, Sky), (1, 1, Sky), (2, 1, Grass)]
               ,[(0, 2, Sky), (1, 2, Grass),  (2, 2, Grass)]]
    }

main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Game" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do
      t <- C.loadTextureWithInfo r "./assets/tiles.png"

      let doRender = renderWorld r t

      _ <- iterateUntilM
        exiting
        (\x ->
          updateWorld x <$> SDL.pollEvents
          >>= \x' -> x' <$ doRender x'
        )
        initialWorld

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
    forM_ (tiles $ tilemap world) $ \row ->
        forM_ row $ \tile ->
          renderTile tile
    where
      tileWidth :: Double
      tileWidth = (fromIntegral $ SDL.textureWidth ti) / 16
      tileRect = C.mkRect 0 0 tileWidth tileWidth

      getTilesheetCoords :: (Num a) => Tile -> (a, a)
      getTilesheetCoords Sky = (16, 16)
      getTilesheetCoords Grass = (80, 16)

      renderTile (x, y, t)
        = SDL.copy renderer texture
            (Just $ floor <$> tileRect `moveTo` getTilesheetCoords t)
            (Just $ floor <$> tileRect `moveTo` (fromIntegral x * tileWidth, fromIntegral y * tileWidth))



moveTo :: SDL.Rectangle a -> (a, a) -> SDL.Rectangle a
moveTo (SDL.Rectangle _ d) (x, y) = SDL.Rectangle (C.mkPoint x y) d
