module Input where

import qualified SDL
import Types
import Data.List

payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit
payloadToIntent (SDL.KeyboardEvent k)    = keyEventToIntent k
payloadToIntent _                        = Idle

keyEventToIntent :: SDL.KeyboardEventData -> Intent
keyEventToIntent (SDL.KeyboardEventData _ SDL.Released _ keysym) = 
  case SDL.keysymKeycode keysym of
    SDL.KeycodeA      -> Stop
    SDL.KeycodeD      -> Stop
    _                 -> Idle
keyEventToIntent (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeW      -> Jump
    SDL.KeycodeA      -> MoveLeft
    SDL.KeycodeS      -> Idle
    SDL.KeycodeD      -> MoveRight
    SDL.KeycodeE      -> Attack
    SDL.KeycodeUp     -> Jump
    SDL.KeycodeLeft   -> MoveLeft
    SDL.KeycodeDown   -> Idle
    SDL.KeycodeRight  -> MoveRight
    SDL.KeycodeSpace  -> Jump
    SDL.KeycodeLCtrl  -> Attack
    SDL.KeycodeRCtrl  -> Attack
    _                 -> Idle

type InputHandler = (Intent -> Character -> Character) 

applyIntentToGameState :: InputHandler -> Intent -> GameState -> GameState
applyIntentToGameState applyIntent i state = 
    state { world = (world state) { playerCharacter = applyIntent i . playerCharacter . world $ state } }

handleInput :: InputHandler -> GameState -> [SDL.Event] -> GameState
handleInput applyIntent w
  = foldl' (flip (applyIntentToGameState applyIntent)) w . fmap (payloadToIntent . SDL.eventPayload)
