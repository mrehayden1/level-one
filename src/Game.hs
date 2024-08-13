module Game (
  Env(..),

  Output(..),
  World(..),

  start,
  fps
) where

import Control.Monad
import Reflex

import Game.Env
import Game.Menu

data Output t = Output {
  outputPicture :: Behavior t Picture,
  outputQuit :: Event t ()
}

-- The state of our game's simulation which updates once per frame
data World = World {
  worldBlocks :: [Path],
  worldEntrance :: Point,
  worldExit :: Point,
  worldFrameNumber :: Int -- for debugging
}

start :: Game t m => Event t () -> Event t InputEvent -> m (Output t)
start eTick eInput = do
  sprites <- asks envSprites

  let eQuit = void . ffilter (isKey (SpecialKey KeyEsc) Down) $ eInput

  --eWorld <- game eTick eInput
  --let worldPicture = fmap (render sprites) eWorld

  picture <- pure . fmap (Scale scale' scale') <=< menu eTick $ eInput
  --    picture = (\a b -> Pictures [a, b]) <$> worldPicture <*> menuPicture

  return . Output (current picture) $ eQuit

worldEntrance' :: Point
worldEntrance' = (-175, 0)

worldExit' :: Point
worldExit' = (175, 0)

worldBlocks' :: [Path]
worldBlocks' =
  let x' = gameWidth' / 2
      y' = gameHeight' / 2
  in [
       [      ( x',  y'), (2 *   x' ,  y'), (2 *   x' , -y'),       ( x', -y')],
       [      (-x',  y'), (2 * (-x'),  y'), (2 * (-x'), -y'),       (-x', -y')],
       [((-2) * x', -16), (2 *   x' , -16), (2 *    x', -y'), ((-2) * x', -y')],
       [((-2) * x',  32), (2 *   x' ,  32), (2 *    x',  y'), ((-2) * x',  y')]
     ]

render :: Sprites -> World -> Picture
render Sprites{..} World{..} =
  Pictures [
      uncurry renderDoor worldEntrance,
      uncurry renderDoor worldExit,
      renderBlocks worldBlocks
    ]
 where
  renderDoor x y =
    Translate (x * scale') y . Scale scale' scale' . staticSprite $ spritesDoor

  renderBlocks = Color colourFg . Pictures . fmap renderBlock

  renderBlock = Polygon . fmap (\(a, b) -> (a * scale', b * scale'))
