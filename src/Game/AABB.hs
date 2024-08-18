module Game.AABB (
  HasAABB(..),

  AABB(..),
  Origin,
  Dimensions,

  translate,

  Collision(..),
  collisionNormal,
  collisionTangent,

  collision,
  colliding,

  grounded,

  render
) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Function
import Data.List (sortBy)
import Data.Maybe

import Game.Env.Render hiding (Point)
import Game.Linear

import Debug.Trace

data AABB = AABB {
  orig :: Origin Float,
  dims :: Dimensions Float
} deriving (Show)

_orig :: Lens' AABB (Origin Float)
_orig = lens orig (\bb o -> bb { orig = o })

_dims :: Lens' AABB (Dimensions Float)
_dims = lens dims (\bb d -> bb { dims = d })

translate :: AABB -> V2 Float -> AABB
translate (AABB o s) t = AABB (o + P t) s

minkowskiDifference :: AABB -> AABB -> AABB
minkowskiDifference (AABB p1 d1) (AABB p2 d2) =
  let P (V2 x1 y1) = p1
      P (V2 x2 y2) = p2
      V2 w2 h2     = d2
 in AABB (P (V2 (x1 - x2 - w2) (y1 - y2 - h2))) (d1 + d2)

-- Invariant: Dimensions of an AABB are positive
class HasAABB a where
  aabb :: a -> AABB

instance HasAABB AABB where
  aabb = id

instance (HasAABB a, HasAABB b) => HasAABB (a, b) where
  aabb (a, b) =
    let AABB (P (V2 x  y )) (V2 w  h ) = aabb a
        AABB (P (V2 x' y')) (V2 w' h') = aabb b
        xMin = min x x'
        yMin = min y y'
        xMax = max (x + w) (x' + w')
        yMax = max (y + h) (y' + h')
    in AABB (P (V2 xMin yMin)) (V2 (xMax - xMin) (yMax - yMin))

data Collision =
    -- Immediate collision detected
    CollisionDiscr (V2 Float) -- penetration vector into the object
    -- Collision detected along an object's current path
  | CollisionCont  (V2 Float) -- collision normal
                   Float      -- distance along path collision occurs
 deriving (Show, Eq)

instance Ord Collision where
  compare (CollisionDiscr p ) (CollisionDiscr q ) = compare p q
  compare (CollisionCont _ d) (CollisionCont _ e) = compare d e
  compare (CollisionDiscr _ ) (CollisionCont _ _) = LT
  compare (CollisionCont _ _) (CollisionDiscr _ ) = GT


-- collisionNormal - get the unit vector which signifies the axis in which a
-- collision occured
collisionNormal :: Collision -> V2 Float
collisionNormal (CollisionDiscr pen  ) = signorm pen
collisionNormal (CollisionCont  n   _) = n

-- collisionTangent - get the unit vector which signifies the line along which
-- a collision occured
collisionTangent :: Collision -> V2 Float
collisionTangent (CollisionDiscr (V2 x y)  ) = signorm $ V2 (-y) x
collisionTangent (CollisionCont  (V2 x y) _) = V2 (-y) x

-- collision - Continuous collision detection.
-- Given a subject, displacement vector, and object, compute the collision.
-- If colliding on the path this will be a collision normal and collision time
-- as a fraction of the displacement vector, a penetration vector if they're
-- already colliding, or otherwise Nothing.
collision :: (HasAABB a, HasAABB b)
  => a
  -> b
  -> V2 Float
  -> Maybe Collision
collision sub obj disp =
  -- discreet collision detection - are subject and object already colliding.
  let discreet = fmap CollisionDiscr . colliding sub $ obj
  -- continuous collision detection
      continuous = do
        -- broad phase check i.e. is the combined aabb of the subject and
        -- subject destination colliding with the object
        let sub' = translate (aabb sub) disp -- displace the subject
        _ <- colliding (sub, sub') obj
        -- detect a future collision by checking if the origin ever enters the
        -- minkowski difference of the two AABBs which is moving with the
        -- relative velocity of the subject.
        let (AABB (P (V2 ox oy)) (V2 w h)) =
              minkowskiDifference (aabb sub) . aabb $ obj
            -- line segments that make up the minkowski difference AABB's edges
            ps = [P (V2  ox       oy     ), P (V2  ox      (oy + h)),
                  P (V2 (ox + w) (oy + h)), P (V2 (ox + w)  oy     )]
            ss = take 4 . zip ps . drop 1 . cycle $ ps
            -- Collision normal of each segment for collision responses
            -- (pointing out of the object bounding box).
            -- i.e. multiplying the velociy by the collision tangent to get the
            -- new velocity.
            ns = fmap (normal . unP . signorm . uncurry (flip subtract)) ss
        -- Find the collision normal and fraction of the path at which the
        -- collision occurs.
        listToMaybe . fmap (uncurry CollisionCont) . sortBy (compare `on` snd)
          . catMaybes . zipWith (fmap . (,)) ns . fmap intersection' $ ss
  in discreet <|> continuous
 where
  normal (V2 x y) = V2 (-y) x

  intersection' :: (P2 Float, P2 Float) -> Maybe Float
  intersection' = fmap fst . uncurry (intersection origin (-P disp))

-- colliding - Discreet collision detection.
-- Given a two objects, if they are colliding compute the penetration vector
-- (the shortest vector one of the objects needs to be traslated by so they no
-- longer collide.)
colliding :: (HasAABB s, HasAABB o) => s -> o -> Maybe (V2 Float)
colliding sub obj = do
  let AABB (P (V2 x y)) (V2 w h) = minkowskiDifference (aabb sub) . aabb $ obj
  -- Check the origin is in the Minkowski difference AABB
  guard $ x < 0 && y < 0 && x + w > 0 && y + h > 0
  listToMaybe . sortBy (compare `on` quadrance)
    $ [V2 x 0, V2 (x + w) 0, V2 0 y, V2 0 (y + h)]

-- grounded - Is the subject touching the top surface of the object.
grounded :: (HasAABB s, HasAABB o) => s -> o -> Bool
grounded sub obj =
  let AABB (P (V2 x y)) (V2 w _) = minkowskiDifference (aabb sub) . aabb $ obj
  in y == 0 && x <= 0 && 0 <= w

-- interscetion of two line segments as a fraction of each segment
-- adpated from here https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
intersection :: P2 Float
  -> P2 Float
  -> P2 Float
  -> P2 Float
  -> Maybe (Float, Float)
intersection p1 p2 q1 q2 = do
  let r = p2 - p1
      s = q2 - q1
      tN = unP (q1 - p1) `crossZ` unP s
      tD = unP r `crossZ` unP s
      uN = unP (q1 - p1) `crossZ` unP r
      t = tN / tD
      u = uN / tD
  -- co-linear lines don't intersect
  guard $ tD /= 0 || uN /= 0
  -- parallel lines, don't intersect
  guard $ tD /= 0
  -- finally check for intersection
  guard $ 0 <= t && t <= 1 && 0 <= u && u <= 1
  return (t, u)

render :: AABB -> Picture
render AABB{..} =
  let P (V2 x y) = orig
      V2 w h     = dims
  in Line [(x, y), (x + w, y), (x + w, y + h), (x, y + h), (x, y)]
