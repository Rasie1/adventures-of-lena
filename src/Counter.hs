-- Counter module based on code from https://github.com/Rydgel/flappy-haskell

{-# LANGUAGE OverloadedStrings #-}

module Counter where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Text           (Text)
import           Data.Word
import           Foreign.C.Types
import           Data.List           (genericLength)
import           Linear              hiding (identity)
import           Linear.Affine
import           Prelude             hiding (init)
import           SDL                 (($=))
import qualified SDL
import qualified Data.HashMap.Strict as M

import           Common
import           Types

destroyDigitsTextures :: DigitsTextures -> IO ()
destroyDigitsTextures = mapM_ (SDL.destroyTexture . fst)

renderDigit :: SDL.Renderer -> DigitsTextures -> Char -> Point V2 CInt -> IO ()
renderDigit r dt c = renderTexture r $ (M.!) dt c

renderScore :: SDL.Renderer -> DigitsTextures -> Int -> IO ()
renderScore r dt scoreInt = do
  let stringScore = show scoreInt
      totalWidth  = 28.0 * genericLength stringScore
      start       = 28
  foldM_ (\z c -> renderDigit r dt c (P (V2 z 25)) >> return (z+28)) start stringScore

renderTexture :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> Point V2 CInt -> IO ()
renderTexture r (t, ti) xy =
  SDL.copy r t Nothing (Just $ SDL.Rectangle xy (getTextureSize ti))
