module Types where

import Data.Array

type ScreenSize = (Double, Double)
type Camera = (Double, Double)
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
    }
data MovePosition = MovingLeft | MovingRight | NotMoving
data Controller = Controller
    { port :: Int,
      actions :: [Intent]
    }
data Intent
  = Idle
  | MoveLeft
  | MoveRight
  | Jump
  | Attack
  | Action
  | Quit


data Level = Level
    { tiles :: Array (Int, Int) Tile
    } deriving Show
data Tile = Sky | Grass | Player | Enemy deriving Show

data World = World
  { level :: Level
  , characters :: [Character]
  }
