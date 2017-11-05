module Input where

import qualified SDL
import GameState
import Data.Foldable          

data Intent
  = Idle
  | Quit

handleInput :: GameState -> [SDL.Event] -> GameState
handleInput w
  = foldl' (flip applyIntent) w
  . fmap (payloadToIntent . SDL.eventPayload)

payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit
payloadToIntent (SDL.MouseMotionEvent e) = motionIntent e
payloadToIntent (SDL.MouseButtonEvent e) = buttonIntent e
payloadToIntent _                        = Idle

motionIntent :: SDL.MouseMotionEventData -> Intent
motionIntent _ = Idle
-- motionIntent e = Hover q
--   where
--     q = selectQuadrant x y
--     (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e


  -- | SDL.mouseButtonEventMotion e == SDL.Pressed -> Down
  --
buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent _ = Idle
-- buttonIntent e = t q
--   where
--     q = selectQuadrant x y
--     (SDL.P (SDL.V2 x y)) = SDL.mouseButtonEventPos e
--     t = if SDL.mouseButtonEventMotion e == SDL.Pressed
--            then Press
--            else Release

applyIntent :: Intent -> GameState -> GameState
applyIntent Idle        = idleGameState
applyIntent Quit        = quitGameState
