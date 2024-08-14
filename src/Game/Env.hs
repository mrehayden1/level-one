module Game.Env (
  module Control.Monad.Reader,

  fps,
  fps',
  deltaT,

  GLFW.Key(..),
  GLFW.KeyState(..),
  GLFW.ModifierKeys(..),

  Game,

  Env(..),
  initialise,
  cleanup,

  module Game.Env.Audio,

  module Game.Env.Render,

  Window,
  Reflex.WindowReflexes(..),
  displayPicture,
  pollEvents
) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Graphics.UI.GLFW as GLFW
import Reflex
import qualified Reflex.GLFW.Simple as Reflex


import Game.Env.Audio hiding (cleanup, initialise)
import qualified Game.Env.Audio as Audio
import Game.Env.Render hiding (initialise, loadFont, loadSprites)
import qualified Game.Env.Render as Render
import Game.Env.Window hiding (initialise, displayPicture, pollEvents)
import qualified Game.Env.Window as Window

-- Resolution of the simulation - i.e. how many frames per second.
fps :: Int
fps = 24

fps' :: Float
fps' = fromIntegral fps

deltaT :: Float
deltaT = recip fps'

type Game t m = (MonadIO m, MonadIO (Performable m), MonadReader (Env t) m,
  MonadFix m, MonadHold t m, Adjustable t m, NotReady t m , PerformEvent t m,
  PostBuild t m, TriggerEvent t m)

data Env t = Env {
  envAudio :: Audio,
  envFont :: Font,
  envSprites :: Sprites,
  envWindow :: Window,
  envWindowReflexes :: Reflex.WindowReflexes t
}

initialise :: (Reflex t, MonadHold t m, TriggerEvent t m, MonadIO m)
  => String
  -> m (Env t)
initialise windowTitle = do
  liftIO $ putStrLn "Loading sprites..."
  sprites <- liftIO Render.loadSprites

  liftIO $ putStrLn "Loading fonts..."
  font <- liftIO Render.loadFont

  liftIO $ putStrLn "Initialising audio..."
  audio <- liftIO Audio.initialise

  window <- liftIO $ Window.initialise windowTitle
  windowReflexes <- Reflex.windowReflexes . glfwWindow $ window

  return . Env audio font sprites window $ windowReflexes

cleanup :: Env t -> IO ()
cleanup Env{..} = do
  Audio.cleanup envAudio

displayPicture :: Window -> Picture -> IO ()
displayPicture = Window.displayPicture

pollEvents :: IO ()
pollEvents = Window.pollEvents
