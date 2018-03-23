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
import Audio

import qualified SDL
import Data.Maybe
import qualified Data.Text
import Data.Foldable          
import Control.Monad
import System.Clock
import qualified Data.HashMap.Strict as Map

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
      music <- loadMusic "./assets/level1.ogg"
      playMusic music

      tilesTexture <- loadTextureWithInfo r "./assets/tiles2.png"
      characterTexture <- loadTextureWithInfo r "./assets/lena_brown.png"
      enemyTexture <- loadTextureWithInfo r "./assets/enemy.png"
      digitsTexture <- loadDigitsTextures r
      initialTime <- getTime Monotonic

      let unitSize = (fromIntegral $ SDL.textureWidth (snd tilesTexture)) / 24
      let enemySprites = 
                     [("RunRight", 
                      SpriteInfo { framesCount  = 8
                                 , currentFrame = 0
                                 , frameCoords  = (0, 0)
                                 , frameSize    = (48, 48)
                                 , unitSize     = unitSize
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = False
                                 }),
                     ("RunLeft", 
                      SpriteInfo { framesCount  = 8
                                 , currentFrame = 0
                                 , frameCoords  = (0, 48)
                                 , frameSize    = (48, 48)
                                 , unitSize     = unitSize
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = False
                                 }),
                     ("FallRight", 
                      SpriteInfo { framesCount  = 8
                                 , currentFrame = 0
                                 , frameCoords  = (0, 0)
                                 , frameSize    = (48, 48)
                                 , unitSize     = unitSize
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = False
                                 }),
                     ("FallLeft", 
                      SpriteInfo { framesCount  = 8
                                 , currentFrame = 0
                                 , frameCoords  = (0, 48)
                                 , frameSize    = (48, 48)
                                 , unitSize     = unitSize
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = False
                                 }),
                     ("Stand", 
                      SpriteInfo { framesCount  = 8
                                 , currentFrame = 0
                                 , frameCoords  = (0, 48)
                                 , frameSize    = (48, 48)
                                 , unitSize     = unitSize
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = False
                                 })
                     ]

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
                     ("Stand", 
                      SpriteInfo { framesCount  = 1
                                 , currentFrame = 0
                                 , frameCoords  = (0, 0)
                                 , frameSize    = (48, 54)
                                 , unitSize     = unitSize
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 1
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
                                 }),
                     ("FallLeft",  
                      SpriteInfo { framesCount  = 4
                                 , currentFrame = 0
                                 , frameCoords  = (569, 60)
                                 , frameSize    = (45, 54)
                                 , unitSize     = unitSize
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = True
                                 }),
                     ("FallRight",  
                      SpriteInfo { framesCount  = 4
                                 , currentFrame = 0
                                 , frameCoords  = (48, 60)
                                 , frameSize    = (45, 54)
                                 , unitSize     = unitSize
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = True
                                 })
                    ]
      let characterSpriteSheet = SpriteSheet { sprites = Map.fromList sprites
                                             , currentSprite       = "RunLeft"
                                             , spriteSheetTexture  = characterTexture
                                             , spriteSheetPosition = (0, 0)
                                             }
      let enemySpriteSheet = SpriteSheet { sprites = Map.fromList enemySprites
                                         , currentSprite       = "RunLeft"
                                         , spriteSheetTexture  = enemyTexture
                                         , spriteSheetPosition = (0, 0)
                                         }

      currentLevel <- loadLevelByName r "menu" tilesTexture unitSize

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
                            >>= processLevelTransition r tilesTexture characterSpriteSheet enemySpriteSheet unitSize
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
                            then do music <- loadMusic ("./assets/" ++ dname ++ ".ogg")
                                    playMusic music
                            else return ()
                        return state { world = mkWorld nextLevel characterTex enemyTex }
        Nothing   -> return state

loadLevelByName :: SDL.Renderer -> String -> (SDL.Texture, SDL.TextureInfo) -> Double -> IO Level
loadLevelByName r name tilesTexture unitSize = do
      backgroundTexture <- loadTextureWithInfo r ("./assets/" ++ name ++ ".png")
      let mapPath = "./assets/" ++ name ++ ".map" 
      levelString <- readFile mapPath
      return (loadLevel levelString tilesTexture backgroundTexture unitSize)

printDebugData :: GameState -> String
printDebugData state = 
    "FPS: " ++ show (framesSinceLastFPSPrint state) 
    -- ++ ", ₽" ++ (show . money . world) state 
    -- ++ ", ec: " ++ show collision
    --         where (p:enemies) = characters . world $ state
    --               collides e = distance (currentPosition p) (currentPosition e) < (radius p + radius e)
    --               collision = or . map collides $ enemies


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


loadDigitsTextures :: SDL.Renderer -> IO DigitsTextures
loadDigitsTextures r = mapM (loadTextureWithInfo r) $
  Map.fromList [ ('0', "assets/font_big_0.png")
               , ('1', "assets/font_big_1.png")
               , ('2', "assets/font_big_2.png")
               , ('3', "assets/font_big_3.png")
               , ('4', "assets/font_big_4.png")
               , ('5', "assets/font_big_5.png")
               , ('6', "assets/font_big_6.png")
               , ('7', "assets/font_big_7.png")
               , ('8', "assets/font_big_8.png")
               , ('9', "assets/font_big_9.png")
               ]
