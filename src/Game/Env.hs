module Game.Env (
  fps,
  fps',
  deltaT,

  Game,

  Env(..),
  initialise,
  cleanup,

  InputEvent,
  pattern G.EventKey,
  G.Key(..),
  G.SpecialKey(..),
  G.KeyState(..),
  isKey,

  G.playIO,

  module Control.Monad.Reader,
  module Game.Env.Audio,
  module Game.Env.Render
) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Graphics.Gloss.Interface.IO.Game as G
import Reflex

import Game.Env.Audio hiding (cleanup, initialise)
import qualified Game.Env.Audio as Audio
import Game.Env.Render hiding (loadFont, loadSprites)
import qualified Game.Env.Render as Render

-- Resolution of the simulation - i.e. how many frames per second.
fps :: Int
fps = 24

fps' :: Float
fps' = fromIntegral fps

deltaT :: Float
deltaT = recip fps'

type InputEvent = G.Event

isKey :: G.Key -> G.KeyState -> InputEvent -> Bool
isKey k s (G.EventKey k' s' _ _) = k == k' && s == s'
isKey _ _ _                      = False

type Game t m = (MonadIO m, MonadReader Env m, MonadFix m, MonadHold t m,
  Adjustable t m, NotReady t m , PerformEvent t m, PostBuild t m)

data Env = Env {
  envAudio :: Audio,
  envFont :: Font,
  envSprites :: Sprites
}

initialise :: IO Env
initialise = do
  putStrLn "Initialising audio..."
  audio <- Audio.initialise

  putStrLn "Loading sprites..."
  sprites <- Render.loadSprites

  putStrLn "Loading fonts..."
  font <- Render.loadFont

  return $ Env audio font sprites

cleanup :: Env -> IO ()
cleanup Env{..} = do
  Audio.cleanup envAudio
