module Types where

import Data.Array

type Camera = (Double, Double)
type Position = (Double, Double)
type Speed = (Double, Double)
type DeltaTime = Double

data Character = Character
    { moveSpeed  :: Double
    , radius     :: Double
    , inertia    :: Double
    , jumpHeight :: Double

    , currentPosition :: Position
    , currentSpeed    :: Speed

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
