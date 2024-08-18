module Game.Player (
  Output(..),
  player,

  Facing(..),
) where

import Control.Monad
import Data.Bool
import Data.List (foldl', sort)
import Data.Maybe
import Linear
import Reflex hiding (mapMaybe)
import Reflex.Network

import qualified Game.AABB as AABB
import Game.AABB hiding (render)
import Game.Block hiding (render)
import Game.Env
import Game.Linear
import Game.Util

import Debug.Trace

newtype Player = Player (P2 Float)

-- Represents the extent of the player's head and feet for vertical collisions.
instance HasAABB Player where
  aabb (Player (P (V2 x y))) = AABB (P (V2 (x - 2) y)) (V2 4 25)

newtype PlayerArms = PlayerArms (P2 Float)

-- Represents the extent of the player's arms for horizontal collisions.
instance HasAABB PlayerArms where
  aabb (PlayerArms (P (V2 x y))) = AABB (P (V2 (x - 3) (y + 1))) (V2 6 23)

data Animation = Standing | Walking

data Facing = FLeft | FRight
  deriving (Show, Eq)


data Output t = Output {
  outFacing :: Dynamic t Facing,
  outPicture :: Dynamic t Picture,
  outPosition :: Dynamic t (P2 Float),
  outVelocity :: Dynamic t (V2 Float)
}


player :: forall t m. Game t m
  => P2 Float
  -> Facing
  -> [Block]
  -> Event t Float
  -> m (Output t)
player p0 facingStart blocks eTick = do
  WindowReflexes{..} <- asks envWindowReflexes
  sprites <- asks envSprites

  -- Input
  leftHeld <- holdUniqDyn <=< mkKeyDownDyn $ Key'Left
  rightHeld <- holdUniqDyn <=< mkKeyDownDyn $ Key'Right

  -- Direction
  let moveDirection =
        (+) . bool @Float 0 (-1) <$> leftHeld <*> (bool 0 1 <$> rightHeld)

  rec
    -- Position and velocity
    -- The next position or velocity is influenced by collision so we check for
    -- any current collision or collisions happening during this frame.
    positionVelocity <- holdDyn (p0, 0) $ positionVelocity'
      <$> current moveDirection
      <*> current position
      <*> current velocity
      <*> pure blocks
      <@> eTick

    let position = fmap fst positionVelocity
        velocity = fmap snd positionVelocity

  -- Grounded
  grnd <- holdUniqDyn $ grounded' <$> fmap Player position <*> pure blocks

  -- Facing - distinct from direction, points in the last direction the player
  -- moved
  facing <- holdDyn facingStart . fmap (bool FLeft FRight . (> 0))
    . ffilter (/= 0) . updated $ moveDirection

  -- Animation
  animation <- fmap join . networkHold (standing eTick)
    . ffor (updated $ (,) <$> moveDirection <*> grnd) $ \(dir, gr) ->
        if not gr || dir == 0 then standing eTick else walking eTick

  -- Sound
  audio animation

  -- Render
  let picture = liftA3 (render sprites) facing animation position

  return $ Output facing picture position velocity
 where
  positionVelocity' :: Float
    -> P2 Float
    -> V2 Float
    -> [Block]
    -> Float
    -> (P2 Float, V2 Float)
  positionVelocity' dir pos (V2 _ vy) bs dt =
    let v = V2 (dir * walkingSpeed) (vy + g * dt)
        cs = collision' pos (v ^* dt) bs
        (pos', vel', f) = foldl' doCollision (pos, v, 0) cs
    in (pos' + P (vel' ^* (dt * (1 - f))), vel')
   where
    doCollision :: (P2 Float, V2 Float, Float)
      -> Collision
      -> (P2 Float, V2 Float, Float)
    doCollision (p, v, f) c@(CollisionDiscr pen) =
      let t = collisionTangent c
      in (p - P pen, v * abs t, f)
    doCollision (p, v, f) c@(CollisionCont _ f') =
      let p' = p + P (v ^* (dt * (f' - f)))
          t = collisionTangent c
      in (p', v * abs t, f')

  -- Calculate the collision of the player in the current frame.
  collision' :: P2 Float
    -> V2 Float
    -> [Block]
    -> [Collision]
  collision' pos disp bs =
    -- Do horizontal and vertical collisions separately so we don't snag on
    -- corners.
    let vcs = filter ((== V2 0 1) . abs . collisionNormal)
                . mapMaybe (flip (collision (Player pos)) disp) $ bs
        hcs = filter ((== V2 1 0) . abs . collisionNormal)
                . mapMaybe (flip (collision (PlayerArms pos)) disp) $ bs
        cs = sort $ vcs ++ hcs
    in cs

  grounded' :: Player -> [Block] -> Bool
  grounded' = any . grounded

  audio :: Dynamic t (Animation, Int) -> m ()
  audio animation = do
    _ <- networkView . fmap playFootstep $ animation
    return ()
   where
    playFootstep (Standing, _) =
      return ()
    playFootstep (Walking,  x) =
      when (x == 15) $ asks envAudio >>= playSound

  standing = return . fmap ((Standing, ) . (`div` 48))
               <=< foldDyn (\_ f -> (f + 1) `mod` 96) 48

  walking  = return . fmap (Walking, )
               <=< foldDyn (\_ f -> (f + 1) `mod` 16) 9

jumpAcceleration :: Float
jumpAcceleration = 157.5 -- arbitrary number that feels good.

-- g - acceleration due to gravity in pixels per second.
g :: Float
g = -175 -- ~10m/s^2

-- walkingSpeed - in pixels per second
-- Character stride is 11px every 16 frames
walkingSpeed :: Float
walkingSpeed = 16.5 -- @24fps = 16.5

render :: Sprites
  -> Facing
  -> (Animation, Int)
  -> P2 Float
  -> Picture
render Sprites{..} facing (animName, animFrame) p@(P (V2 x y)) =
  Pictures [
      picture,
      Color debugOutlineColour . AABB.render . aabb . Player $ p,
      Color debugOutlineColour . AABB.render . aabb . PlayerArms $ p
    ]
 where
  floor' = fromIntegral . (floor :: Float -> Int)

  picture = Translate (floor' x) (floor' y)
    . Scale (bool (-1) 1 (facing == FRight)) 1
    . Pictures
    $ [
        sprite (animFrame + frameOffset) spritesPlayer
      ]

  -- TODO - implementation an animation system, maybe using aseprite metadata
  frameOffset =
    case animName of
      Standing -> 0
      Walking  -> 2
