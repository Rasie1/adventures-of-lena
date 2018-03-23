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


loadDigitsTextures :: SDL.Renderer -> IO DigitsTextures
loadDigitsTextures r = mapM (loadTextureWithInfo r) $
  Map.fromList [ ('0', obfuscatedStrings ! "font_big_0.png")
               , ('1', obfuscatedStrings ! "font_big_1.png")
               , ('2', obfuscatedStrings ! "font_big_2.png")
               , ('3', obfuscatedStrings ! "font_big_3.png")
               , ('4', obfuscatedStrings ! "font_big_4.png")
               , ('5', obfuscatedStrings ! "font_big_5.png")
               , ('6', obfuscatedStrings ! "font_big_6.png")
               , ('7', obfuscatedStrings ! "font_big_7.png")
               , ('8', obfuscatedStrings ! "font_big_8.png")
               , ('9', obfuscatedStrings ! "font_big_9.png")
               ]

levelsMusic :: Map.HashMap String String
levelsMusic = Map.fromList [ ("level1", obfuscatedStrings ! "farewell_mona_lisa.ogg"),
                             ("level2", obfuscatedStrings ! "43burnt.ogg"),
                             ("level3", obfuscatedStrings ! "black_bubblegum.ogg"),
                             ("level4", obfuscatedStrings ! "43burnt.ogg"),
                             ("level5", obfuscatedStrings ! "panasonic_youth.ogg"),
                             ("level6", obfuscatedStrings ! "panasonic_youth.ogg"),
                             ("level7", obfuscatedStrings ! "farewell_mona_lisa.ogg")
                            ]

obfuscatedStrings :: Map.HashMap String String
obfuscatedStrings = Map.fromList [ ("level1.png",             "assets/data.bin.5"),
                                   ("level1.map",             "assets/data.bin.7"),
                                   ("level2.map",             "assets/data.bin.6"),
                                   ("level2.png",             "assets/data.bin.8"),
                                   ("level3.map",             "assets/data.bin.9"),
                                   ("level3.png",             "assets/data.bin.27"),
                                   ("level4.map",             "assets/data.bin.26"),
                                   ("level4.png",             "assets/data.bin.17"),
                                   ("level5.map",             "assets/data.bin.18"),
                                   ("level5.png",             "assets/data.bin.15"),
                                   ("level6.map",             "assets/data.bin.25"),
                                   ("level6.png",             "assets/data.bin.31"),
                                   ("level7.map",             "assets/data.bin.12"),
                                   ("level7.png",             "assets/data.bin.24"),
                                   ("menu.map",               "assets/data.bin.32"),
                                   ("menu.png",               "assets/data.bin.2"),
                                   ("farewell_mona_lisa.ogg", "assets/data.bin.0"),
                                   ("43burnt.ogg",            "assets/data.bin.4"),
                                   ("black_bubblegum.ogg",    "assets/data.bin.3"),
                                   ("lena_brown.png",         "assets/data.bin.23"),
                                   ("lena_red.png",           "assets/data.bin.30"),
                                   ("lena_green.png",         "assets/data.bin.14"),
                                   ("lena_blue.png",          "assets/data.bin.1"),
                                   ("tiles.png",              "assets/data.bin.35"),
                                   ("enemy.png",              "assets/data.bin.11"),
                                   ("save.bin",               "assets/data.bin.33"),
                                   ("font_big_0.png",         "assets/data.bin.16"),
                                   ("font_big_1.png",         "assets/data.bin.19"),
                                   ("font_big_2.png",         "assets/data.bin.28"),
                                   ("font_big_3.png",         "assets/data.bin.10"),
                                   ("font_big_4.png",         "assets/data.bin.22"),
                                   ("font_big_5.png",         "assets/data.bin.29"),
                                   ("font_big_6.png",         "assets/data.bin.13"),
                                   ("font_big_7.png",         "assets/data.bin.20"),
                                   ("font_big_8.png",         "assets/data.bin.34"),
                                   ("font_big_9.png",         "assets/data.bin.21"),
                                   ("panasonic_youth.ogg",     "assets/data.bin.36")
                                  ]


