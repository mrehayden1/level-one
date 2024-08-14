module Main (main) where

import Control.Concurrent
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Dependent.Sum
import Data.Foldable
import Data.Function
import Data.IORef
import Data.Maybe
import Data.Traversable
import Reflex hiding (mapMaybe)
import Reflex.Host.Class
import System.Exit

import Game
import Game.Env as Env

version :: String
version = "0.1.0"

name :: String
name = "No Exit"

main :: IO ()
main = do
  putStrLn   ""
  putStrLn $ name ++ " — version " ++ version ++ " ..."
  putStrLn   ""
  putStrLn   "Copyright Categorical Industries 2024, All Rights Reserved."
  putStrLn   ""

  putStrLn "Starting..."

  eventsChan <- newChan

  runSpiderHost $ do
    -- Create the environment which contains the audio, fonts, sprites, window,
    -- Gloss state and also WindowReflexes which contains Window events.
    -- Pass the event Chan so `reflex-glfw-simple` can do its thing.
    env@Env{..} <- flip runTriggerEventT eventsChan . Env.initialise $ name

    -- Create post build event.
    (ePostBuild, postBuildTriggerRef) <- newEventWithTriggerRef

    -- Create the network.
    (Output{..}, FireCommand fire) <- hostPerformEventT
      . flip runPostBuildT ePostBuild
      . flip runTriggerEventT eventsChan
      . flip runReaderT env
      $ start

    -- Event handles - for reading current `Event` values, which may or may not
    -- have been produced since the last frame.
    hQuit <- subscribeEvent outputQuit
    hPicture <- subscribeEvent outputPicture

    -- Exit callback which cleans up and shuts down successfully.
    let exit = do
          putStrLn "Exiting..."
          Env.cleanup env
          exitSuccess

    -- Processes event outputs. There will be a list of outputs, one for each
    -- event triggered so we have to process them all accordingly.
    let handleOutput outs = do
          -- Any quit event should cause the application to exit.
          mapM_ (\_ -> liftIO exit) . mapMaybe fst $ outs
          -- Render the current Picture output (the last one).
          mapM_ (liftIO . displayPicture envWindow) . listToMaybe
            . mapMaybe snd . reverse $ outs

    -- Reads output event handles and sequences any actions by PerformEvent
    -- methods.
    let readPhase = do
          quit <- sequence =<< readEvent hQuit
          picture <- sequence =<< readEvent hPicture
          return (quit, picture)

    -- Trigger an event and processes the outputs of the reflex network.
    let fireAndProcess triggerRef value = do
          maybeTrigger <- liftIO $ readIORef triggerRef
          out <- case maybeTrigger of
            -- If nothing is listening don't do anything
            Nothing      -> return []
            Just trigger -> fire [trigger :=> Identity value] readPhase
          handleOutput out

    -- Triggers events created by our network using `TriggerEvent`, so things
    -- like the tick and window events.
    let fireEventTriggerRefs ers rcb = do
          mes <- liftIO $
            for ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
              me <- readIORef er
              pure $ fmap (==> a) me
          a <- fire (catMaybes mes) rcb
          liftIO $ for_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
          pure a

    -- Trigger the PostBuild event and react to any initial output (if any).
    fireAndProcess postBuildTriggerRef ()

    -- Main loop.
    fix $ \loop -> do
      -- Poll for events which will collect any `TriggerEvent` events created
      -- by window callbacks by reflex-glfw-simple.
      liftIO pollEvents
      -- Read the event channel written to by TriggerEvent methods and process
      -- the outputs.
      events <- liftIO $ readChan eventsChan
      outs <- fireEventTriggerRefs events readPhase
      handleOutput outs
      loop
