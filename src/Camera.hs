module Camera where
import qualified SDL
import Common
import Types

applyCamera :: ScreenSize -> Double -> Camera -> SDL.Rectangle Double -> SDL.Rectangle Double
applyCamera (screenx, screeny) unitSize (x, y) = moveBy ((-x * unitSize, 
                                                          -y * unitSize) `pointPlus` (screenx / 2,
                                                                                      screeny / 2))
