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
import Network.HTTP
import Network.URI
import Config

import qualified SDL
import Data.Foldable          
import Control.Monad
import System.Clock
import Data.HashMap.Strict ((!))
import Control.Exception   (catch, SomeException)

diffTime :: TimeSpec -> TimeSpec -> DeltaTime
diffTime end start = (* 1e-9) $ fromIntegral $ toNanoSecs end - toNanoSecs  start

main :: IO ()
main = withSDL $ withSDLImage $ do
  setHintQuality
  withConfig $ \(Config res scoreUrl) -> do
    withWindow "Game" (floorTuple res) $ \w -> do
        withRenderer w $ \r -> do
          initAudio
          music <- loadMusic (obfuscatedStrings ! "farewell_mona_lisa.ogg")
          playMusic music

          tilesTexture <- loadTextureWithInfo r (obfuscatedStrings ! "tiles.png")
          playerTexture <- loadTextureWithInfo r (obfuscatedStrings ! "lena_brown.png")
          redPlayerTexture <- loadTextureWithInfo r (obfuscatedStrings ! "lena_red.png")
          bluePlayerTexture <- loadTextureWithInfo r (obfuscatedStrings ! "lena_blue.png")
          greenPlayerTexture <- loadTextureWithInfo r (obfuscatedStrings ! "lena_green.png")
          enemyTexture <- loadTextureWithInfo r (obfuscatedStrings ! "enemy.png")
          digitsTexture <- loadDigitsTextures r
          initialTime <- getTime Monotonic

          let playerSpriteSheet = SpriteSheet { sprites = playerSprites
                                              , currentSprite       = "RunLeft"
                                              , spriteSheetTexture  = playerTexture
                                              , spriteSheetPosition = (0, 0)
                                              }
          let redPlayerSpriteSheet = SpriteSheet { sprites = playerSprites
                                                 , currentSprite       = "RunLeft"
                                                 , spriteSheetTexture  = redPlayerTexture
                                                 , spriteSheetPosition = (0, 0)
                                                 }
          let bluePlayerSpriteSheet = SpriteSheet { sprites = playerSprites
                                                  , currentSprite       = "RunLeft"
                                                  , spriteSheetTexture  = bluePlayerTexture
                                                  , spriteSheetPosition = (0, 0)
                                                  }
          let greenPlayerSpriteSheet = SpriteSheet { sprites = playerSprites
                                                 , currentSprite       = "RunLeft"
                                                 , spriteSheetTexture  = greenPlayerTexture
                                                 , spriteSheetPosition = (0, 0)
                                                 }
          let enemySpriteSheet = SpriteSheet { sprites = enemySprites
                                             , currentSprite       = "RunLeft"
                                             , spriteSheetTexture  = enemyTexture
                                             , spriteSheetPosition = (0, 0)
                                             }

          currentLevel <- loadLevelByName r "menu" tilesTexture unit

          let initialGameState = mkGameState (mkWorld currentLevel playerSpriteSheet 
                                                                   redPlayerSpriteSheet
                                                                   greenPlayerSpriteSheet
                                                                   bluePlayerSpriteSheet 
                                                                   enemySpriteSheet) initialTime digitsTexture
          let updateTime time state = return state { currentTime = time }
          let processFPS time state = if diffTime time (lastFPSPrintTime state) > 1.0
                                         then do putStrLn $ printDebugData state
                                                 return state { framesSinceLastFPSPrint = 0
                                                              , lastFPSPrintTime = time }
                                         else return state { framesSinceLastFPSPrint = framesSinceLastFPSPrint state + 1 }
          let processLevelTransition :: GameState -> IO GameState
              processLevelTransition state = 
                    case wantToChangeLevel . world $ state of
                        Just name -> do nextLevel <- loadLevelByName r name tilesTexture unit
                                        if name /= "level1" && name /= "menu"
                                            then do music <- loadMusic (levelsMusic ! name)
                                                    playMusic music
                                            else return ()
                                        if finishedLevel . world $ state 
                                            then reportScore scoreUrl (levelName . level . world $ state) name (money . world $ state)
                                            else return ()
                                        return state { world = mkWorld nextLevel playerSpriteSheet redPlayerSpriteSheet greenPlayerSpriteSheet bluePlayerSpriteSheet enemySpriteSheet
                                                     , camera = Camera { cameraPosition = (0, 0)
                                                                       , oldCameraEdge  = (0, 0)
                                                                       , armLength      = (0, 2)
                                                                       , oldPivot       = (0, 0)
                                                                       , pivotOffset    = (0, -1)
                                                                       } }
                        Nothing   -> return state



          let update x = do time <- getTime Monotonic
                            state <- handleInput applyIntent x <$> SDL.pollEvents
                            processFPS time state
                                >>= updateTime time
                                >>= updateGame (diffTime time (currentTime state))
                                >>= processLevelTransition
                                >>= renderFrame res r 
                                  . updateCamera

          runApp update initialGameState

          SDL.destroyTexture (fst tilesTexture)


reportScore :: Network.URI.URI -> String -> String -> Int -> IO ()
reportScore url from to score = 
    do putStrLn $ "Sending score: " ++ show score ++ ", " ++ from ++ " -> " ++ to
       let fullUri = show url ++ "?score=" ++ show score ++ "&from=" ++ from ++ "&to=" ++ to
       putStrLn ("Requesting " ++ show url)
       catch (do result <- Network.HTTP.simpleHTTP (getRequest (fullUri))
                 return ())
             (\e -> putStrLn ("Error sending scores! Error: " ++ show (e :: SomeException)))


loadLevelByName :: SDL.Renderer -> String -> (SDL.Texture, SDL.TextureInfo) -> Double -> IO Level
loadLevelByName r name tilesTexture unitSize = do
      backgroundTexture <- loadTextureWithInfo r (obfuscatedStrings ! (name ++ ".png"))
      let mapPath = (obfuscatedStrings ! (name ++ ".map")) 
      levelString <- readFile mapPath
      return (loadLevel levelString name tilesTexture backgroundTexture unitSize)

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
