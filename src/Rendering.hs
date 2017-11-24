module Rendering where

import qualified SDL

import qualified SDL.Image

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text              (Text)
import Data.Maybe
import Data.Foldable          

import SDL (($=))
import Common

import Control.Monad

import Types
import GameState
import Drawable

renderFrame :: ScreenSize -> SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> GameState -> IO (GameState)
renderFrame screen renderer texture gameState = do
    SDL.clear renderer
    render screen 
          (camera gameState)
           renderer 
           texture 
          (world gameState)
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
