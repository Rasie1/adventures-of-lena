module World where

import Common
import Level
import Drawable
import Data.Maybe
import Character
import Types
import Data.Array
import Data.List
import Bot

instance Drawable World where
    render s c r world = do render s c r (level world)
                            mapM_ (render s c r) (playerCharacter world : enemyCharacters world)


updateWorld :: Double -> World -> World
updateWorld dt = processPlayerDeath
               . processTiles
               . updateCharacters dt

mkWorld :: Level -> SpriteSheet -> SpriteSheet -> World
mkWorld lvl playerSpriteSheet enemySpriteSheet = saveWorld World 
    { level = lvl
    , enemyCharacters = spawnEnemies
    , playerCharacter = spawnPlayer
    , money = 0
    , savedWorld = Nothing
    , wantToChangeLevel = Nothing
    }
    where tileToEnemy ((x, y), Enemy) = 
                Just ((enemy enemySpriteSheet) { currentPosition = (fromIntegral x, fromIntegral y) })
          tileToEnemy _ = Nothing
          tileToPlayer ((x, y), Player) = 
                Just ((player playerSpriteSheet) { currentPosition = (fromIntegral x, fromIntegral y) })
          tileToPlayer _ = Nothing
          spawnEnemies = mapMaybe tileToEnemy (assocs (tiles lvl))
          spawnPlayer = head $ mapMaybe tileToPlayer (assocs (tiles lvl))

saveWorld :: World -> World
saveWorld w = w { savedWorld = Just w }

loadWorld :: World -> World
loadWorld w = case savedWorld w of
                    Just save -> saveWorld save
                    Nothing   -> w

processPlayerDeath :: World -> World
processPlayerDeath w = let enemies = enemyCharacters w
                           p       = playerCharacter w
                           collides e = distance (currentPosition p) (currentPosition e) < (radius p{- + radius e-})
                        in if or . map collides $ enemies then loadWorld w
                                                          else w


processTiles :: World -> World
processTiles w =  let p = playerCharacter w
                      t = tiles $ level w
                      toCoord (x, y) = (floor x, floor y)
                      pos = currentPosition p
                      tile = getTile t (toCoord pos) 
                   in case (tile, isDeadly tile) of 
                        (_, True) -> loadWorld w
                        (Level1, _) -> w { wantToChangeLevel = Just "level1" }
                        (Level2, _) -> w { wantToChangeLevel = Just "level2" }
                        (Level3, _) -> w { wantToChangeLevel = Just "level3" }
                        (Level4, _) -> w { wantToChangeLevel = Just "level4" }
                        (Level5, _) -> w { wantToChangeLevel = Just "level5" }
                        (Level6, _) -> w { wantToChangeLevel = Just "level6" }
                        (Level7, _) -> w { wantToChangeLevel = Just "level7" }
                        (Menu, _) -> w { wantToChangeLevel = Just "menu" }
                        (Money, _) -> addMoney w { level = removeTile (toCoord pos) (level w) }
                        _     -> w

moneyMultiplier :: Int
moneyMultiplier = 25

addMoney :: World -> World
addMoney w = w { money = money w + moneyMultiplier }

updateCharacters :: Double -> World -> World
updateCharacters dt w = w { enemyCharacters = mapMaybe (updateCharacter dt w) (enemyCharacters w)
                          , playerCharacter = fromJust $ updateCharacter dt w (playerCharacter w) }


anyCharacter spriteSheet = Character 
    { moveVelocity = 3
    , radius       = 0.5
    , inertia      = 0.75
    , airInertia   = 0.99
    , jumpPower    = 1

    , currentPosition = (0, 0)
    , currentVelocity = (0, 0)

    , characterController = Controller { port = 0, actions = [], bot = Nothing }

    , moving    = NotMoving
    , falling   = True
    , using     = False
    , attacking = False

    , jumping   = False
    , canJump    = True
    , timeToJump = 1.0

    , characterSpriteSheet = spriteSheet
    }

player = anyCharacter

enemy spriteSheet = (anyCharacter spriteSheet) { characterController = Controller { port = 1, actions = [], bot = Just simpleMoveBot } }
