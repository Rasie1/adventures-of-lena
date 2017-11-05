module World where

import Level
import Drawable

data World = World
  { level :: Level
  }

instance Drawable World where
    render world = render level
