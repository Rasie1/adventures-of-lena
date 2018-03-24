{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Common
import Camera
import Level
import World
import GameState
import Types
import Input
import Character
import Rendering
import Audio
import Data

import qualified SDL
import Data.Foldable          
import Control.Monad
import System.Clock
import Data.HashMap.Strict ((!))

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
  withWindow "Game" resolution $ \w -> do
    withRenderer w $ \r -> do
      initAudio
      music <- loadMusic (obfuscatedStrings ! "farewell_mona_lisa.ogg")
      playMusic music

      tilesTexture <- loadTextureWithInfo r (obfuscatedStrings ! "tiles.png")
      characterTexture <- loadTextureWithInfo r (obfuscatedStrings ! "lena_brown.png")
      enemyTexture <- loadTextureWithInfo r (obfuscatedStrings ! "enemy.png")
      digitsTexture <- loadDigitsTextures r
      initialTime <- getTime Monotonic

      let characterSpriteSheet = SpriteSheet { sprites = playerSprites
                                             , currentSprite       = "RunLeft"
                                             , spriteSheetTexture  = characterTexture
                                             , spriteSheetPosition = (0, 0)
                                             }
      let enemySpriteSheet = SpriteSheet { sprites = enemySprites
                                         , currentSprite       = "RunLeft"
                                         , spriteSheetTexture  = enemyTexture
                                         , spriteSheetPosition = (0, 0)
                                         }

      currentLevel <- loadLevelByName r "menu" tilesTexture unit

      let initialGameState = mkGameState (mkWorld currentLevel characterSpriteSheet enemySpriteSheet) initialTime digitsTexture
      let updateTime time state = return state { currentTime = time }
      let processFPS time state = if diffTime time (lastFPSPrintTime state) > 1.0
                                     then do putStrLn $ printDebugData state
                                             return state { framesSinceLastFPSPrint = 0
                                                          , lastFPSPrintTime = time }
                                     else return state { framesSinceLastFPSPrint = framesSinceLastFPSPrint state + 1 }



      let update x = do time <- getTime Monotonic
                        state <- handleInput x <$> SDL.pollEvents
                        processFPS time state
                            >>= updateTime time
                            >>= updateGame (diffTime time (currentTime state))
                            >>= processLevelTransition r tilesTexture characterSpriteSheet enemySpriteSheet unit
                            >>= renderFrame resolutionDouble r 
                              . updateCamera

      runApp update initialGameState

      SDL.destroyTexture (fst tilesTexture)

processLevelTransition :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo)
                  -> SpriteSheet -> SpriteSheet -> Double
                   -> GameState -> IO GameState
processLevelTransition r tex characterTex enemyTex s state = 
    case wantToChangeLevel . world $ state of
        Just name -> do nextLevel <- loadLevelByName r name tex s
                        if name /= "level1" && name /= "menu"
                            then do music <- loadMusic (levelsMusic ! name)
                                    playMusic music
                            else return ()
                        return state { world = mkWorld nextLevel characterTex enemyTex
                                     , camera = Camera { cameraPosition = (0, 0)
                                                       , oldCameraEdge  = (0, 0)
                                                       , armLength      = (0, 4)
                                                       , oldPivot       = (0, 0)
                                                       , pivotOffset    = (0, -2)
                                                       } }
        Nothing   -> return state

loadLevelByName :: SDL.Renderer -> String -> (SDL.Texture, SDL.TextureInfo) -> Double -> IO Level
loadLevelByName r name tilesTexture unitSize = do
      backgroundTexture <- loadTextureWithInfo r (obfuscatedStrings ! (name ++ ".png"))
      let mapPath = (obfuscatedStrings ! (name ++ ".map")) 
      levelString <- readFile mapPath
      return (loadLevel levelString tilesTexture backgroundTexture unitSize)

printDebugData :: GameState -> String
printDebugData state = 
    "FPS: " ++ show (framesSinceLastFPSPrint state) 

runApp :: (Monad m) => (GameState -> m GameState) -> GameState -> m ()
runApp update = repeatUntil update shutdown

repeatUntil :: (Monad m) => (a -> m a) -> (a -> Bool) -> a -> m ()
repeatUntil f p = go
  where go a = f a >>= \b -> unless (p b) (go b)

updateGame :: DeltaTime -> GameState -> IO GameState
updateGame dt state = return state { world = updateWorld dt $ world state }

applyIntentToGameState :: Intent -> GameState -> GameState
applyIntentToGameState i state = 
    state { world = (world state) { playerCharacter = applyIntent i . playerCharacter . world $ state } }

handleInput :: GameState -> [SDL.Event] -> GameState
handleInput w
  = foldl' (flip applyIntentToGameState) w . fmap (payloadToIntent . SDL.eventPayload)
