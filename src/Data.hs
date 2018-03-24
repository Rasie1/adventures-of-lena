module Data where

import Types
import Rendering
import qualified SDL
import Data.HashMap.Strict ((!))
import Data.HashMap.Strict as Map
import Data.Text

unit :: Double
unit = 48

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



enemySprites :: Map.HashMap Text SpriteInfo
enemySprites = Map.fromList [("RunRight", 
                      SpriteInfo { framesCount  = 8
                                 , currentFrame = 0
                                 , frameCoords  = (0, 0)
                                 , frameSize    = (48, 48)
                                 , unitSize     = unit
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
                                 , unitSize     = unit
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
                                 , unitSize     = unit
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
                                 , unitSize     = unit
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = False
                                 }),
                     ("StandLeft", 
                      SpriteInfo { framesCount  = 8
                                 , currentFrame = 0
                                 , frameCoords  = (0, 48)
                                 , frameSize    = (48, 48)
                                 , unitSize     = unit
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = False
                                 }),
                     ("StandRight", 
                      SpriteInfo { framesCount  = 8
                                 , currentFrame = 0
                                 , frameCoords  = (0, 48)
                                 , frameSize    = (48, 48)
                                 , unitSize     = unit
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = False
                                 })
                     ]

playerSprites :: Map.HashMap Text SpriteInfo
playerSprites = Map.fromList [("RunRight", 
                      SpriteInfo { framesCount  = 4
                                 , currentFrame = 0
                                 , frameCoords  = (48, 0)
                                 , frameSize    = (48, 54)
                                 , unitSize     = unit
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = False
                                 }),
                     ("StandRight", 
                      SpriteInfo { framesCount  = 1
                                 , currentFrame = 0
                                 , frameCoords  = (0, 0)
                                 , frameSize    = (48, 54)
                                 , unitSize     = unit
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 1
                                 , timeSinceChange   = 0
                                 , reversedFrames    = False
                                 }),
                     ("StandLeft", 
                      SpriteInfo { framesCount  = 1
                                 , currentFrame = 0
                                 , frameCoords  = (0, 0)
                                 , frameSize    = (48, 54)
                                 , unitSize     = unit
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
                                 , unitSize     = unit
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
                                 , unitSize     = unit
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
                                 , unitSize     = unit
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = True
                                 }),
                     ("AttackRight",  
                      SpriteInfo { framesCount  = 4
                                 , currentFrame = 0
                                 , frameCoords  = (569, 60)
                                 , frameSize    = (45, 54)
                                 , unitSize     = unit
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = True
                                 }),
                     ("AttackLeft",  
                      SpriteInfo { framesCount  = 4
                                 , currentFrame = 0
                                 , frameCoords  = (48, 60)
                                 , frameSize    = (45, 54)
                                 , unitSize     = unit
                                 , gapBetweenFrames  = 0
                                 , frameChangeTime   = 0.2
                                 , timeSinceChange   = 0
                                 , reversedFrames    = True
                                 })
                    ]
