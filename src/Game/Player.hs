module Game.Player (
  PlayerOutput(..),
  player,

  Facing(..),
) where

import Control.Monad
import Data.Bool
import Reflex
import Reflex.Network

import Game.Env
import Util

data Animation = Standing | Walking

data Facing = FLeft | FRight
  deriving (Show, Eq)

data PlayerOutput t = PlayerOutput {
  playerPicture :: Dynamic t Picture,
  playerPosition :: Dynamic t Point,
  playerFacing :: Dynamic t Facing
}

player :: forall t m. Game t m
  => Point
  -> Facing
  -> Event t ()
  -> m (PlayerOutput t)
player (xStart, yStart) facingStart eTick = do
  WindowReflexes{..} <- asks envWindowReflexes

  leftHeld <- holdUniqDyn <=< mkKeyDownDyn $ Key'Left
  rightHeld <- holdUniqDyn <=< mkKeyDownDyn $ Key'Right

  let heldDirInput = liftA2 (,) leftHeld rightHeld
      isMoving = fmap (uncurry (||)) heldDirInput
  animation <- fmap join . networkHold (standing eTick)
    . ffor (updated isMoving) $ \m ->
        if m then walking eTick else standing eTick
  let eVx = fmap velocityX . tag (current heldDirInput) $ eTick
  facing <- holdDyn facingStart . fmap (bool FLeft FRight . (> 0))
    . ffilter (/= 0) $ eVx
  x <- foldDyn (+) xStart eVx
  let y = constDyn yStart
      position = liftA2 (,) x y
  audio animation
  sprites <- asks envSprites
  let picture = liftA4 (render sprites) facing animation x y
  return $ PlayerOutput picture position facing
 where
  audio :: Dynamic t (Animation, Int) -> m ()
  audio animation = do
    _ <- networkView . fmap playFootstep $ animation
    return ()
   where
    playFootstep (Standing, _) =
      return ()
    playFootstep (Walking,  x) =
      when (x == 15) $ asks envAudio >>= playSound

  standing = return . fmap ((Standing,) . (`div` 48))
               <=< foldDyn (\_ f -> (f + 1) `mod` 96) 48

  walking = return . fmap (Walking,) <=< foldDyn (\_ f -> (f + 1) `mod` 16) 9

velocityX :: (Bool, Bool) -> Float
velocityX (l, r) =
  vx / fps'
 where
  vx = if l then -walkingSpeed else 0
         + if r then walkingSpeed else 0

jumpAcceleration :: Float
jumpAcceleration = 157.5 -- arbitrary number that feels good.

-- g - acceleration due to gravity in pixels per second.
g :: Float
g = -350 -- ~20m/s^2, twice normal gravity, which feels too floaty.

-- walkingSpeed - in pixels per second
-- Character stride is 11px every 16 frames
walkingSpeed :: Float
walkingSpeed = 11 * fps'/16 -- @24fps = 16.5

render :: Sprites -> Facing -> (Animation, Int) -> Float -> Float -> Picture
render Sprites{..} facing (animName, animFrame) x y =
  Translate (floor' x) (floor' y)
    . Scale (bool (-1) 1 (facing == FRight)) 1
    . sprite (animFrame + frameOffset)
    $ spritesPlayer
 where
  floor' = fromIntegral . (floor :: Float -> Int)

  -- TODO - implementation an animation system, maybe using aseprite metadata
  frameOffset =
    case animName of
      Standing -> 0
      Walking  -> 2
