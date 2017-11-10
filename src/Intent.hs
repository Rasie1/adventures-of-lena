module Intent where

import qualified SDL

data Intent
  = Idle
  | MoveLeft
  | MoveRight
  | Jump
  | Attack
  | Action
  | Quit
