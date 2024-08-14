module Game.Env.Audio (
  Audio,
  initialise,
  cleanup,

  playSound
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Random
import Sound.ALUT

data Audio = Audio {
  audioQueue :: TChan Source,
  audioDevice :: Device,
  audioFootstep1 :: Source,
  audioFootstep2 :: Source
}

initialise :: IO Audio
initialise = withProgNameAndArgs runALUTUsingCurrentContext $ \_ _ -> do
  (Just device) <- openDevice Nothing
  (Just context) <- createContext device []
  currentContext $= Just context
  [source1, source2] <- genObjectNames 2
  footstep1 <- createBuffer (File "sound/footstep.wav")
  footstep2 <- createBuffer (File "sound/footstep2.wav")
  queueBuffers source1 [footstep1]
  queueBuffers source2 [footstep2]
  audio <- Audio
    <$> newTChanIO
    <*> pure device
    <*> pure source1
    <*> pure source2

  _ <- forkOS $ loop audio
  return audio

loop :: Audio -> IO ()
loop Audio{..} = loop'
 where
  loop' = do
    -- polling interval (in microseconds)
    threadDelay 50000 -- 50ms
    mSound <- atomically $ tryReadTChan audioQueue
    case mSound of
      Nothing    -> loop'
      Just sound -> do
        play [sound]
        loop'

cleanup :: Audio -> IO ()
cleanup Audio{..} = do
  _ <- closeDevice audioDevice
  return ()

playSound :: MonadIO m => Audio -> m ()
playSound Audio{..} = do
  (step :: Int) <- randomRIO (0, 1)
  let sound = case step of
        0 -> audioFootstep1
        1 -> audioFootstep2
        _ -> error "playSound: index out of range"
  pitch' <- randomRIO (0.95, 1.05)
  pitch sound $= pitch'
  liftIO . atomically . writeTChan audioQueue $ sound
