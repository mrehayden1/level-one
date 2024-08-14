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
  outputPicture :: Event t Picture,
  outputQuit :: Event t ()
}

-- The state of our game's simulation which updates once per frame
data World = World {
  worldBlocks :: [Path],
  worldEntrance :: Point,
  worldExit :: Point,
  worldFrameNumber :: Int -- for debugging
}

start :: Game t m => m (Output t)
start = do
  eTick <- fmap void . tickLossyFromPostBuildTime $ (1 /) . fromIntegral $ fps

  WindowReflexes{..} <- asks envWindowReflexes

  let eQuit = void . ffilter (\(k, _, _, _) -> k == Key'Escape) $ key
  --eQuit <- pure . void . updated <=< mkKeyDownDyn $ Key'Escape

  --eWorld <- game eTick eInput
  --let worldPicture = fmap (render sprites) eWorld
  --    picture = (\a b -> Pictures [a, b]) <$> worldPicture <*> menuPicture

  menuPicture <- menu eTick
  let picture = updated
        . fmap (\p -> Scale scale' scale' . Pictures $ [background, p])
        $ menuPicture

  -- Debugging square
  --picture <- pure . current <=< holdDyn Blank . ffor eTick
  --  $ \_ -> Polygon [(-50,-50), (-50, 50), (50, 50), (50, -50)]

  return $ Output picture eQuit

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
