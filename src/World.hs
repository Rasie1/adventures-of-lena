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
import System.Random

instance Drawable World where
    render s c r world = do shakeCamera <- if (timeSinceAttack . playerCharacter $ world) < 0.1
                                                 then do 
                                                         r1 <- randomRIO (0, 2) :: IO Double
                                                         r2 <- randomRIO (0, 2) :: IO Double
                                                         return c { cameraPosition = cameraPosition c `pointPlus` (r1 - 1, 
                                                                                                                   r2 - 1) }
                                                 else return c
                            render s shakeCamera r (level world)
                            mapM_ (render s shakeCamera r) (playerCharacter world : enemyCharacters world)


updateWorld :: Double -> World -> World
updateWorld dt = processPlayerDeath
               . processPlayerAttacks
               . processTiles
               . updateCharacters dt

mkWorld :: Level -> SpriteSheet -> SpriteSheet -> SpriteSheet -> SpriteSheet -> SpriteSheet -> World
mkWorld lvl playerSpriteSheet redPlayerSpriteSheet greenPlayerSpriteSheet bluePlayerSpriteSheet enemySpriteSheet = saveWorld World 
    { level = lvl
    , enemyCharacters = spawnEnemies
    , playerCharacter = spawnPlayer
    , money = 0
    , savedWorld = Nothing
    , wantToChangeLevel = Nothing
    , finishedLevel = False
    , redPlayerSpriteSheet = redPlayerSpriteSheet
    , greenPlayerSpriteSheet = greenPlayerSpriteSheet
    , bluePlayerSpriteSheet = bluePlayerSpriteSheet
    }
    where tileToEnemy ((x, y), Enemy) = 
                Just ((enemy enemySpriteSheet) { currentPosition = (fromIntegral x, fromIntegral y) })
          tileToEnemy _ = Nothing
          tileToPlayer ((x, y), Player) = 
                Just ((mkCharacter playerSpriteSheet) { currentPosition = (fromIntegral x, fromIntegral y) })
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
processPlayerAttacks :: World -> World
processPlayerAttacks w = let enemies = enemyCharacters w
                             p       = playerCharacter w
                             toCoord (x, y) = (floor x, floor y)
                             collides e = distance (currentPosition p) (currentPosition e) < (radius p + 1.0) 
                             dies e = if collides e && attacking p
                                             then Nothing
                                             else Just e 
                          in w { enemyCharacters = mapMaybe dies $ enemies
                               , playerCharacter = p { attacking = False }
                               , level = foldl (\lvl pos -> replaceTile Money (toCoord pos) lvl) (level w) 
                                       . map currentPosition 
                                       . filter (isNothing . dies) 
                                       $ enemies }


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
                        (Win, _) -> w { wantToChangeLevel = Just "menu"
                                      , finishedLevel = True }
                        (Money, _) -> addMoney w { level = removeTile (toCoord pos) (level w) }
                        (RedDye, _) -> w { playerCharacter = applyDye (redPlayerSpriteSheet w) Red (playerCharacter w) 
                                         , level = removeTile (toCoord pos) (level w) }
                        (BlueDye, _) -> w { playerCharacter = applyDye (bluePlayerSpriteSheet w) Blue (playerCharacter w) 
                                          , level = removeTile (toCoord pos) (level w) }
                        (Seva, _) -> w { level = removeTile (toCoord pos) (level w) }
                        _     -> w

applyDye :: SpriteSheet -> Color -> Character -> Character
applyDye s Red c = c { canAttack = True
                     , moveVelocity = 4.75
                     , jumpPower = 1.25
                     , characterSpriteSheet = s } 
applyDye s Green c = c
applyDye s Blue c = c { moveVelocity = 6
                      , jumpPower = 0.6
                      , canAttack = False
                      , characterSpriteSheet = s
                      , flyMode = True }
applyDye _ _ c = c

moneyMultiplier :: String -> Int
moneyMultiplier name = if name == "level4" then 10
                                           else 25

addMoney :: World -> World
addMoney w = w { money = money w + moneyMultiplier (levelName . level $ w) }

updateCharacters :: Double -> World -> World
updateCharacters dt w = w { enemyCharacters = mapMaybe (updateCharacter dt w) (enemyCharacters w)
                          , playerCharacter = fromJust $ updateCharacter dt w (playerCharacter w) }

