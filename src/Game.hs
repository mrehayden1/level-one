module Game (
  Env(..),

  Output(..),
  World(..),

  start
) where

import Control.Monad
import Reflex
import Reflex.Network

import Game.Env
import qualified Game.Level as Level
import Game.Menu hiding (Output)
import qualified Game.Menu as Menu

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

-- Resolution of the simulation - i.e. how many frames per second.
fps :: Float
fps = 24

start :: Game t m => m (Output t)
start = do
  -- Simulation tick rate, in ticks per second.
  -- Change this down to slow down the simulation for debugging.
  let tickRate = fps

  -- Tick event which produces the delta t once every tick
  -- (constant since we have a fixed FPS).
  eTick <- pure . fmap (const (1 / fps)) <=< tickLossyFromPostBuildTime
    . realToFrac . (1 /) $ (tickRate :: Float)

  rec
    levelOut <- networkHold (level eTick 1) . fmap (level eTick)
                     . updated $ levelNo
    levelNo <- holdDyn 0 . switchDyn . fmap Level.outEChangeLevel $ levelOut

  let picture = Level.outPicture =<< levelOut
      eQuit   = switchDyn . fmap Level.outEQuit $ levelOut

  let picture' = updated
        . fmap (\p -> Scale scale' scale' . Pictures $ [background, p])
        $ picture

  return $ Output picture' eQuit

level :: Game t m => Event t Float -> Int -> m (Level.Output t)
level eTick 0 = do
  output <- menu eTick
  let picture = Menu.outPicture output
      eQuit = Menu.outEQuit output
      eStart = 1 <$ Menu.outEStart output
  return . Level.Output eQuit eStart $ picture
level eTick 1 = Level.one eTick
level _     n = error $ "Level " ++ show n ++ " doesn't exist."
