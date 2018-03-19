module Types where

import Data.Array
import System.Clock
import qualified SDL

type ScreenSize = (Double, Double)
type Position = (Double, Double)
type Velocity = (Double, Double)
type DeltaTime = Double

data Character = Character
    { moveVelocity :: Double
    , radius       :: Double
    , inertia      :: Double
    , jumpPower   :: Double

    , currentPosition :: Position
    , currentVelocity :: Velocity

    , characterController :: Controller

    , moving    :: MovePosition
    , falling   :: Bool
    , using     :: Bool
    , attacking :: Bool
    , jumping   :: Bool

    , characterSprite :: Sprite
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
    , levelUnitSize :: Double
    }
data Tile = Sky | Grass | Player | Enemy deriving Show

data World = World
  { level :: Level
  , characters :: [Character]
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
  }

data Sprite = Sprite 
  { framesCount  :: Int
  , currentFrame :: Int
  , frameCoords  :: (Int, Int)
  , frameSize    :: (Int, Int)
  , unitSize    :: Double
  , gapBetweenFrames :: Int
  , frameChangeTime :: Double
  , timeSinceChange :: Double
  , spriteTexture :: (SDL.Texture, SDL.TextureInfo)
  , spritePosition :: Position
  }
