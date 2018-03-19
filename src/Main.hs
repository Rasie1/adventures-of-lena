{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Common
import Camera
import Level
import World
import GameState
import Types
import Input
import Actor
import Character
import Rendering
import SpriteSheet

import qualified SDL

import Data.Maybe
import qualified Data.Map
import Data.Text hiding (foldl')
import Data.Foldable          
import Control.Monad

import System.Clock

diffTime :: TimeSpec -> TimeSpec -> DeltaTime
diffTime end start = (* 1e-9) $ fromIntegral $ toNanoSecs end - toNanoSecs  start

screenx :: Int
screenx = 1280
screeny :: Int
screeny = 960
resolution :: (Int, Int)
resolution = (screenx, screeny)
resolutionDouble :: (Double, Double)
resolutionDouble = (fromIntegral screenx, fromIntegral screeny)

main :: IO ()
main = withSDL $ withSDLImage $ do
  setHintQuality
  withWindow "Game" resolution $ \w ->
    withRenderer w $ \r -> do
      let outputScale = 1.0

      levelTexture <- loadTextureWithInfo r "./assets/tiles2.png"
      let unitSize = (fromIntegral $ SDL.textureWidth (snd levelTexture)) / 24 * outputScale
      characterTexture <- loadTextureWithInfo r "./assets/lena_brown.png"
      levelString <- readFile "./assets/tiles.map"
      initialTime <- getTime Monotonic


      let sprites = [("RunRight", 
                      SpriteInfo { framesCount  = 4
                                 , currentFrame = 0
                                 , frameCoords  = (48, 0)
                                 , frameSize    = (48, 54)
                                 , unitSize     = unitSize
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = False
                                 }),
                     ("RunLeft",  
                      SpriteInfo { framesCount  = 4
                                 , currentFrame = 0
                                 , frameCoords  = (560, 0)
                                 , frameSize    = (48, 54)
                                 , unitSize     = unitSize
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = True
                                 })
                    ]
      let characterSpriteSheet = SpriteSheet { sprites = Data.Map.fromList sprites
                                             , currentSprite       = "RunLeft"
                                             , spriteSheetTexture  = characterTexture
                                             , spriteSheetPosition = (0, 0)
                                             }

      let initialGameState = mkGameState (mkWorld (loadLevel levelString levelTexture unitSize) characterSpriteSheet) initialTime
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
                            >>= renderFrame resolutionDouble r 
                              . updateCamera

      runApp update initialGameState

      SDL.destroyTexture (fst levelTexture)


runApp :: (Monad m) => (GameState -> m GameState) -> GameState -> m ()
runApp update = repeatUntil update shutdown

repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)

updateGame :: DeltaTime -> GameState -> IO GameState
updateGame dt state = return state { world = updateWorld dt $ world state }

applyIntentToGameState :: Intent -> GameState -> GameState
applyIntentToGameState i s@GameState { world = w@World { characters = (player:xs) } } = 
        s { world = w { characters = (applyIntent i player):xs } }
applyIntentToGameState Quit s = s
applyIntentToGameState _ s    = s

handleInput :: GameState -> [SDL.Event] -> GameState
handleInput w
  = foldl' (flip applyIntentToGameState) w . fmap (payloadToIntent . SDL.eventPayload)
