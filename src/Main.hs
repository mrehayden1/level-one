module Main (main) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Dependent.Sum
import Data.IORef
import Data.StateVar
import qualified Graphics.UI.GLUT.Window as GLUT
import Reflex
import Reflex.Host.Class
import System.Exit

import Game
import Game.Env as Env

version :: String
version = "0.1.0"

name :: String
name = "No Exit: The Game"

displayMode :: Display
displayMode = InWindow name (windowWidth, windowHeight) (0, 0)
--displayMode = FullScreen

main :: IO ()
main = do
  shouldInitDisplayRef <- newIORef True
  -- Initial world state, Nothing before the simulation had begun.
  pictureRef <- newIORef Blank

  putStrLn   ""
  putStrLn $ name ++ " — version " ++ version ++ " ..."
  putStrLn   ""
  putStrLn   "Copyright Categorical Industries 2024, All Rights Reserved."
  putStrLn   ""

  putStrLn "Starting..."

  env <- initialise

  let exit = do
        putStrLn "Exiting..."
        Env.cleanup env
        exitSuccess

  -- Game loop
  runSpiderHost $ do
    (eOpen, openTriggerRef) <- newEventWithTriggerRef
    (eTick, tickTriggerRef) <- newEventWithTriggerRef
    (eInput, inputTriggerRef) <- newEventWithTriggerRef

    (Output{..}, FireCommand fire) <- hostPerformEventT
      . flip runPostBuildT eOpen
      . flip runReaderT env
      $ start eTick eInput

    hQuit <- subscribeEvent outputQuit

    let readPhase = do
          quit <- fmap sequence . readEvent $ hQuit
          picture <- sample outputPicture
          fmap (, Just picture) quit

    let handleOutput (quit, picture) = do
          mapM_ (const $ liftIO exit) quit
          mapM_ (liftIO . writeIORef pictureRef) picture

    let fireAndProcess triggerRef value = do
          maybeTrigger <- liftIO $ readIORef triggerRef
          out <- case maybeTrigger of
            -- If nothing is listening don't do anything
            Nothing      -> return []
            Just trigger -> fire [trigger :=> Identity value] readPhase
          mapM_ handleOutput out

    -- Trigger the PostBuild event.
    fireAndProcess openTriggerRef ()

    liftIO $ playIO displayMode colourBg fps ()
      -- Called by the gloss event loop to render the world so we just read
      -- the World IORef and render that.
      (const $ readIORef pictureRef)
      -- Input event handler.
      (const . runSpiderHost . fireAndProcess inputTriggerRef)
      -- Tick handler
      $ \_ _ -> do
          -- Do some initialisation the first tick after the display has been
          -- created.
          -- TODO Find a better way of doing this (if possible?)
          needsInit <- liftIO $ readIORef shouldInitDisplayRef
          when needsInit . liftIO $ do
            putStrLn "Initialising window..."
            -- Hide the cursor.
            GLUT.cursor $= GLUT.None
            -- Make sure we don't initialise again.
            writeIORef shouldInitDisplayRef False

          runSpiderHost $ fireAndProcess tickTriggerRef ()
