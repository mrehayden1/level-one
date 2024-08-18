module Game.Linear (
  Origin,
  Dimensions,
  P2,

  module Linear,
  module Linear.Affine
) where

import Linear
import Linear.Affine hiding (Point)
import qualified Linear.Affine as Affine

type Origin a = P2 a
type Dimensions a = V2 a
type P2 = Affine.Point V2
