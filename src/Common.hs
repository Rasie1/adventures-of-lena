module Common where

import qualified SDL


isContinue :: Maybe SDL.Event -> Bool
isContinue = maybe True (not . isQuitEvent)


conditionallyRun :: (Monad m) => m a -> Bool -> m Bool
conditionallyRun f True = True <$ f
conditionallyRun _ False = pure False


isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _t SDL.QuitEvent) = True
isQuitEvent _ = False



mkPoint :: a -> a -> SDL.Point SDL.V2 a
mkPoint x y = SDL.P (SDL.V2 x y)


mkRect :: a -> a -> a -> a-> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h

moveBy :: (Num a) => (a, a) -> SDL.Rectangle a -> SDL.Rectangle a
moveBy (dx, dy) (SDL.Rectangle (SDL.P (SDL.V2 x y)) d) = SDL.Rectangle (mkPoint (x + dx) (y + dy)) d

moveTo :: (a, a) -> SDL.Rectangle a -> SDL.Rectangle a
moveTo (x, y) (SDL.Rectangle _ d) = SDL.Rectangle (mkPoint x y) d

pointPlus :: (Double, Double) -> (Double, Double) -> (Double, Double) 
pointPlus (x, y) (a, b) = (x + a, y + b)

getRectPosition :: SDL.Rectangle a -> (a, a)
getRectPosition (SDL.Rectangle (SDL.P (SDL.V2 x y)) d) = (x, y)

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id
