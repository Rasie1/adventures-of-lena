module Camera where
import qualified SDL
import Common
import Types

applyCamera :: Camera -> SDL.Rectangle Double -> SDL.Rectangle Double
applyCamera (x, y) = moveBy (x, y)
