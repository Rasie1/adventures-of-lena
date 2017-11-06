module Camera where
import qualified SDL
import Common

type Camera = (Double, Double)

applyCamera :: Camera -> SDL.Rectangle Double -> SDL.Rectangle Double
applyCamera (x, y) = moveBy (x, y)
