module Game.Block (
  Block(..),

  size,
  size',
  gridCoord,

  render
) where

import qualified Game.AABB as AABB
import Game.AABB hiding (render)
import Game.Env
import Game.Linear

data Block = Block Point Float Float

instance HasAABB Block where
  aabb (Block (ox, oy) w h) = AABB (P (V2 ox oy)) (V2 w h)

render :: Block -> Picture
render b@(Block (ox, oy) w h) =
  Pictures [
      Color fgColour . Polygon $ [
          (ox    , oy    ),
          (ox + w, oy    ),
          (ox + w, oy + h),
          (ox    , oy + h)
        ],
      Color debugOutlineColour . AABB.render . aabb $ b
    ]

-- size - standard block size for grid based layout
size :: Int -- World pixels
size = 20 -- 21 x 13 blocks

size' :: Float
size' = fromIntegral size

gridCoord :: Int -> Float
gridCoord = (size' *) . subtract 0.5 . fromIntegral
