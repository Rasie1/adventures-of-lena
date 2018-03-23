module Types where

import Data.Array
import System.Clock
import qualified SDL
import Data.Text
import qualified Data.HashMap.Strict as Map

type ScreenSize = (Double, Double)
type Position = (Double, Double)
type Velocity = (Double, Double)
type DeltaTime = Double

data Character = Character
    { moveVelocity :: Double
    , radius       :: Double
    , inertia      :: Double
    , airInertia   :: Double
    , jumpPower   :: Double

    , currentPosition :: Position
    , currentVelocity :: Velocity

    , characterController :: Controller

    , moving    :: MovePosition
    , falling   :: Bool
    , using     :: Bool
    , attacking :: Bool

    , jumping   :: Bool
    , canJump   :: Bool
    , timeToJump :: Double

    , characterSpriteSheet :: SpriteSheet
    }
data MovePosition = MovingLeft | MovingRight | NotMoving


type Bot = BotMemory -> World -> Character -> [Intent]
data BotMemory = BotMemory

data Controller = Controller
    { port :: Int
    , actions :: [Intent]
    , bot :: Maybe Bot
    }

data Intent
  = Idle
  | MoveLeft
  | MoveRight
  | Stop
  | Jump
  | Attack
  | Action
  | Quit

data Direction
  = Up
  | Right
  | Down
  | Left

data Level = Level
    { tiles :: Array (Int, Int) Tile
    , levelTexture :: (SDL.Texture, SDL.TextureInfo)
    , backgroundTexture :: (SDL.Texture, SDL.TextureInfo)
    , levelUnitSize :: Double
    }

data Tile = Sky 
          | Spikes1
          | Spikes2
          | GroundTop1
          | GroundTop2
          | GroundThin1
          | GroundThin2
          | GroundTopLeft
          | GroundTopRight
          | GroundCenter1
          | GroundCenter2
          | GroundLeft
          | GroundRight
          | GroundBottomLeft
          | GroundBottomRight
          | GroundBottom1
          | GroundBottom2
          | GroundGrassLeft
          | GroundGrassRight
          | GroundLeftBorder
          | GroundRightBorder
          | Player 
          | Enemy 
          | Money 
          | RedDye 
          | BlueDye 
          | GreenDye 
          | KillZone 
          | BushShort
          | Bush1
          | Bush2
          | Bush3
          | Menu
          | Level1
          | Level2
          | Level3
          | Level4
          | Level5
          | Level6
          | Level7
          deriving Show

type DigitsTextures = Map.HashMap Char (SDL.Texture, SDL.TextureInfo)

data World = World
  { level :: Level
  , enemyCharacters :: [Character]
  , playerCharacter :: Character
  , money  :: Int
  , savedWorld :: Maybe World
  , wantToChangeLevel :: Maybe String
  }

data Camera = Camera 
  { cameraPosition :: Position
  , oldCameraEdge  :: Position
  , armLength      :: Position
  , oldPivot       :: Position
  , pivotOffset    :: Position
  }

data GameState = GameState
  { world       :: World
  , currentTime :: TimeSpec

  , framesSinceLastFPSPrint :: Int
  , lastFPSPrintTime        :: TimeSpec

  , camera :: Camera

  , shutdown :: Bool
  , digitsTextures :: DigitsTextures
  }

data SpriteInfo = SpriteInfo 
  { framesCount  :: Int
  , currentFrame :: Int
  , frameCoords  :: (Int, Int)
  , frameSize    :: (Int, Int)
  , unitSize     :: Double
  , gapBetweenFrames :: Int
  , frameChangeTime  :: Double
  , timeSinceChange  :: Double
  , reversedFrames   :: Bool
  }

data SpriteSheet = SpriteSheet 
  { sprites :: Map.HashMap Text SpriteInfo
  , currentSprite :: Text
  , spriteSheetTexture :: (SDL.Texture, SDL.TextureInfo)
  , spriteSheetPosition :: Position
  }
