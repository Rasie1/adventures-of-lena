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

import qualified SDL

import Data.Maybe
import Data.Foldable          
import Control.Monad

import System.Clock

diffTime :: TimeSpec -> TimeSpec -> DeltaTime
diffTime end start = (* 1e-9) $ fromIntegral $ toNanoSecs end - toNanoSecs  start

screenx = 1280
screeny = 960
resolution = (screenx, screeny)
resolutionDouble = (fromIntegral screenx, fromIntegral screeny)

main :: IO ()
main = withSDL $ withSDLImage $ do
  setHintQuality
  withWindow "Game" resolution $ \w ->
    withRenderer w $ \r -> do
      t <- loadTextureWithInfo r "./assets/tiles.png"
      levelString <- readFile "./assets/tiles.map"

      initialTime <- getTime Monotonic

      let initialGameState = mkGameState (mkWorld (loadLevel levelString)) initialTime
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
                            >>= renderFrame resolutionDouble r t 
                              . updateCamera

      runApp update initialGameState

      SDL.destroyTexture (fst t)


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
