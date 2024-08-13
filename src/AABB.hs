module AABB (
  HasAABB(..),

  AABB(..),
  Origin,
  Dimensions,

  translate,

  collision
) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Function
import Data.List (sortBy)
import Data.Maybe
import Linear
import Linear.Affine

data AABB a = AABB {
  orig :: Origin a,
  dims :: Dimensions a
}

type Origin a = P2 a
type Dimensions a = V2 a
type P2 = Point V2

_orig :: Lens' (AABB a) (Origin a)
_orig = lens orig (\bb o -> bb { orig = o })

_dims :: Lens' (AABB a) (Dimensions a)
_dims = lens dims (\bb d -> bb { dims = d })

translate :: Num a => AABB a -> V2 a -> AABB a
translate (AABB o s) t = AABB (o + P t) s

minkowskiDifference :: Num a => AABB a -> AABB a -> AABB a
minkowskiDifference (AABB p1 d1) (AABB p2 d2) =
  let P (V2 x1 y1) = p1
      P (V2 x2 y2) = p2
      V2 w2 h2     = d2
  in AABB (P (V2 (x1 - x2 - w2) (y1 - y2 - h2))) (d1 + d2)

-- Invariant: Dimensions of an AABB are positive
class HasAABB a b where
  aabb :: a -> AABB b

instance HasAABB (AABB a) a where
  aabb = id

instance (HasAABB a c, HasAABB b c, Num c, Ord c) => HasAABB (a, b) c where
  aabb (a, b) =
    let AABB (P (V2 x  y )) (V2 w  h ) = aabb a
        AABB (P (V2 x' y')) (V2 w' h') = aabb b
        xMin = min x x'
        yMin = min y y'
        xMax = max (x + w) (x' + w')
        yMax = max (y + h) (y' + h')
    in AABB (P (V2 xMin yMin)) (V2 xMax yMax)

-- collision' - Discreet collision detection.
-- Given a two objects, compute the penetration vector (the vector one of the
-- objects needs to be traslated by so they no longer collide.)
collision' :: (HasAABB a c, HasAABB b c, Num c, Ord c) => a -> b -> Maybe (V2 c)
collision' a b = do
  let AABB (P (V2 x y)) (V2 w h) = minkowskiDifference (aabb a) . aabb $ b
  guard $ x > 0 || y > 0 || x + w < 0 || y + h < 0
  listToMaybe . sortBy (compare `on` quadrance)
    $ [V2 x 0, V2 (x + w) 0, V2 0 y, V2 0 (y + h)]

data Collision a =
    -- Immediate collision detected
    CollisionDiscr (V2 a) -- penetration vector
    -- Collision detected along an object's current path
  | CollisionCont  (V2 a) -- collision tangent
                   a      -- distance along path collision occurs

-- collision - Continuous collision detection.
-- Given a subject, displacement vector, and object, compute the collision.
-- If colliding on the path this will be a collision normal and collision time
-- as a fraction of the displacement vector, a penetration vector if they're
-- already colliding, or otherwise Nothing.
collision :: forall a b c. (HasAABB a c, HasAABB b c, Floating c, Ord c)
  => a
  -> V2 c
  -> b
  -> Maybe (Collision c)
collision sub disp obj =
  -- discreet collision detection - are subject and object already colliding.
  (fmap CollisionDiscr . collision' sub $ obj)
    -- continuous collision detection
    <|> do
      -- broad phase check i.e. combined aabb of origin and destination with
      -- aabb of object
      (_ :: V2 c) <- collision' broad' obj'
      -- find the collision if any
      let AABB (P (V2 x y)) (V2 h w) = minkowskiDifference (aabb sub) . aabb
                                         $ obj
          ps = [P (V2 x y), P (V2 x (y + h)), P (V2 (x + w) (y + h)),
                P (V2 (x + w) y)]
          -- line segments that make up the minkowski sum AABB's edges
          ss = take 4 . zip ps . drop 1 . cycle $ ps
          -- Collision tangent of each segment for collision responses,
          -- i.e. multiplying the velociy by the collision tangent to get the
          -- new velocity.
          ts = fmap (unP . signorm . abs . uncurry (flip subtract)) ss
      -- Find the collision tangent and fraction of the path at which the
      -- collision occurs.
      listToMaybe . fmap (uncurry CollisionCont) . sortBy (compare `on` snd)
        . mapMaybe (uncurry calc) . zip ts $ ss
 where
  obj' = aabb obj :: AABB c
  sub' = aabb sub :: AABB c
  broad' = aabb (sub', translate sub' disp) :: AABB c

  calc :: V2 c -> (P2 c, P2 c) -> Maybe (V2 c, c)
  calc t e = do
    p <- fmap fst . uncurry (intersection origin (P disp)) $ e
    return (t, p)

-- interscetion of two line segments as a fraction of each segment
-- adpated from here https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
intersection :: (Fractional c, Ord c)
  => P2 c
  -> P2 c
  -> P2 c
  -> P2 c
  -> Maybe (c, c)
intersection p1 p2 q1 q2 = do
  let r = p2 - p1
      s = q2 - q1
      tN = unP (q1 - p1) `crossZ` unP s
      tD = unP r `crossZ` unP s
      uN = unP (q1 - p1) `crossZ` unP r
      uD = unP s `crossZ` unP r
      t = tN / tD
      u = uN / uD
  -- co-linear lines
  -- TODO find intersection of co-linear lines (do we care?)
  guard $ tD == 0 && uN == 0
  -- parallel lines, can never intersect
  guard $ tD == 0
  -- check for intersection
  if 0 <= t && t <= 1 && 0 <= u && u <= 1
    then return (t, u)
    else empty
