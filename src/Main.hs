module Main (main) where

import Codec.BMP
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Dependent.Sum
import Data.IORef
import Data.StateVar
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game hiding (Event)
import qualified Graphics.Gloss.Interface.IO.Game as G
import qualified Graphics.UI.GLUT.Window as GLUT
import Reflex
import Reflex.Host.Class
import Reflex.Network
import System.Exit

import Audio
import Text
import Util

data Output t = Output {
  outputEPicture :: Event t Picture,
  outputEQuit :: Event t ()
}

data World = World {
  worldFrameNumber :: Int, -- for debugging
  worldPlayer :: Player
}

data Player = Player {
  playerFrame :: Int,
  playerDirection :: Bool,
  playerX :: Float,
  playerY :: Float
}

version :: String
version = "0.1.0"

name :: String
name = "This Game Has One Level"

-- Resolution of the simulation - i.e. how many frames per second.
fps :: Int
fps = 24

fps' :: Float
fps' = fromIntegral fps

-- Notes on scale:
-- Our character is 26px high. At 1.5m that's 17.5 game pixels per metre.

displayMode :: Display
displayMode = InWindow name (windowWidth, windowHeight) (0, 0)
--displayMode = FullScreen

windowHeight, windowWidth :: Int -- Screen pixels
--windowHeight = 1440
--windowWidth = 2560
windowHeight = 1080
windowWidth = 1920

levelHeight, levelWidth :: Int -- World pixels
levelHeight = 260 -- ~15m
levelWidth  = 420 --  24m

levelHeight', levelWidth' :: Float
levelHeight' = fromIntegral levelHeight
levelWidth' = fromIntegral levelWidth

-- Screen pixels per game pixel
-- TODO Rename to something like pixel density?
scale' :: Float
scale' = min (fromIntegral windowWidth  / fromIntegral levelWidth )
             (fromIntegral windowHeight / fromIntegral levelHeight)

colourBg :: Color
colourBg = makeColor (246/255) (117/255) (122/255) 1

colourFg :: Color
colourFg = makeColor (158/255) ( 40/255) ( 53/255) 1

data Env = Env {
  envAudio :: Audio,
  envFont :: Font
}

main :: IO ()
main = start

start :: IO ()
start = do
  shouldInitDisplayRef <- newIORef True
  pictureRef <- newIORef Blank

  putStrLn   ""
  putStrLn $ name ++ " — version " ++ version ++ " ..."
  putStrLn   ""
  putStrLn   "Copyright Categorical Industries 2024, All Rights Reserved."
  putStrLn   ""

  putStrLn "Starting..."

  putStrLn "Initialising audio..."
  audio <- Audio.initialise

  putStrLn "Loading sprites..."
  sprites <- liftA2 Sprites
    (readSprite "sprites/Player.bmp")
    (readSprite "sprites/Door.bmp")

  putStrLn "Loading fonts..."
  font <- loadFont
  let env = Env audio font

  let exit = do
        putStrLn "Exiting..."
        Audio.cleanup audio
        exitSuccess

  runSpiderHost $ do
    (eOpen, openTriggerRef) <- newEventWithTriggerRef
    (eTick, tickTriggerRef) <- newEventWithTriggerRef
    (eInput, inputTriggerRef) <- newEventWithTriggerRef

    (Output{..}, FireCommand fire) <- hostPerformEventT
      . flip runPostBuildT eOpen
      . flip runReaderT audio
      . flip runReaderT env
      $ game sprites eTick eInput

    hPicture <- subscribeEvent outputEPicture
    hQuit <- subscribeEvent outputEQuit

    let readPhase = do
          mPicture <- readEvent hPicture
          mQuit <- readEvent hQuit
          liftA2 (,) (sequence mPicture) . sequence $ mQuit

    let handleOutputs mPicture mQuit = do
          mapM_ (const $ liftIO exit) mQuit
          mapM_ (liftIO . writeIORef pictureRef) mPicture

    let fireAndProcess triggerRef value = do
          maybeTrigger <- liftIO $ readIORef triggerRef
          outputs <- case maybeTrigger of
            -- If nothing is listening don't do anything
            Nothing      -> return []
            Just trigger -> fire [trigger :=> Identity value] readPhase
          mapM_ (uncurry handleOutputs) outputs

    -- Trigger the PostBuild event.
    fireAndProcess openTriggerRef ()

    -- Wrap the game loop in a `try` so we can break out by raising an
    -- exception. (Not sure if this is okay, but it works for now.)
    liftIO $ playIO displayMode colourBg fps ()
      -- Called by the event loop to render the world. Just read the output
      -- IORef and return that.
      (const $ readIORef pictureRef)
      -- Input event handler.
      (const . runSpiderHost . fireAndProcess inputTriggerRef)
      -- Tick handler which we use to trigger our input events and read our
      -- output event handles as well as sequencing any actions performed by
      -- our app.
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
          -- Get the list of event values (multiple since we're using
          -- PerformEventT) returned in a monad that sequences any actions
          -- performed.
          runSpiderHost $ fireAndProcess tickTriggerRef ()

readSprite :: String -> IO BitmapData
readSprite filename = do
  mSprite <- return . fmap bitmapDataOfBMP <=< liftIO . readBMP $ filename
  case mSprite of
    Left err     -> fail $ "Parse error: " ++ show err
    Right sprite -> return sprite

data Sprites = Sprites {
    spritePlayer :: BitmapData,
    spriteDoor :: BitmapData
  }

game :: forall t m. (MonadIO m, MonadReader Env m, MonadFix m, MonadHold t m, Adjustable t m, NotReady t m , PostBuild t m)
  => Sprites
  -> Event t ()
  -> Event t G.Event
  -> m (Output t)
game sprites eTick eInput = do
  let eQuit = void . ffilter (isKey $ SpecialKey KeyEsc) $ eInput
  frameNumber <- foldDyn (const succ) (0 :: Int) eTick
  heldDirInput <- return . current
    <=< foldDyn accumInput (False, False) $ eInput
  walkingFrame <- foldDyn (((`mod` 16) .) . (+)) 0
    . fmap (fromEnum . uncurry (||)) . tag heldDirInput $ eTick
  let eVx = fmap velocityX . tag heldDirInput $ eTick
  dir <- holdDyn True . fmap (> 0) . ffilter (/= 0) $ eVx
  x <- foldDyn (+) xStart eVx
  let y = constDyn 0
      player = liftA4 Player walkingFrame dir x y
      world = World <$> frameNumber <*> player
      picture = fmap (render sprites) world
  audio walkingFrame
  return . Output (updated picture) $ eQuit
 where
  xStart  = -175

  audio :: Dynamic t Int -> m ()
  audio walkingFrame = do
    frame <- holdUniqDyn walkingFrame
    _ <- networkView . fmap playFootstep $ frame
    return ()
   where
    playFootstep x =
      when (x == 0) $ asks envAudio >>= playSound

velocityX :: (Bool, Bool) -> Float
velocityX (l, r) =
  vx / fps'
 where
  vx = if l then -walkingSpeed else 0
         + if r then walkingSpeed else 0

levelFloorY :: Float
levelFloorY = 0

levelCeilY :: Float
levelCeilY = 32

-- Arrest the vertical velocity if colliding and add a bit to push us out of
-- the object colided with.
acceleration :: Float -> Float -> Bool -> Float

acceleration y vy False
  | y <= levelFloorY = (-vy + fps' / 2 * (levelFloorY - y)) * fps'
  | y >= levelCeilY  = (-vy + fps' / 2 * (levelCeilY  - y)) * fps'

acceleration y vy True
  | y <= levelFloorY = (-vy + fps' / 2 * (levelFloorY - y) + jumpAcceleration)
                         * fps'

acceleration _ _  _  = g

jumpAcceleration :: Float
jumpAcceleration = 157.5 -- arbitrary number that feels good.

-- g - acceleration due to gravity in pixels per second.
g :: Float
g = -350 -- ~20m/s^2, twice normal gravity, which feels too floaty.


isKey :: Key -> G.Event -> Bool
isKey k (EventKey k' _ _ _) = k == k'
isKey _ _                   = False

accumInput :: G.Event -> (Bool, Bool) -> (Bool, Bool)
accumInput (EventKey (SpecialKey KeyLeft ) s _ _) (_, r) = (s == Down, r)
accumInput (EventKey (SpecialKey KeyRight) s _ _) (l, _) = (l, s == Down)
accumInput _                                      i      = i

keyIsJump :: G.Event -> Bool
keyIsJump (EventKey (SpecialKey KeySpace) Down _ _) = True
keyIsJump (EventKey (SpecialKey KeyUp   ) Down _ _) = True
keyIsJump _                                         = False

-- walkingSpeed - in pixels per second
-- Character stride is 11px every 16 frames
walkingSpeed :: Float
walkingSpeed = 11 * fps'/16 -- @24fps = 16.5

worldEntrance :: (Float, Float)
worldEntrance = (-175, 0)

worldExit :: (Float, Float)
worldExit = (175, 0)

worldBlocks :: [[(Float, Float)]]
worldBlocks =
  let x' = levelWidth' / 2
      y' = levelHeight' / 2
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
      renderPlayer worldPlayer,
      renderBlocks worldBlocks
    ]
 where
  renderPlayer Player{..} =
    let x = floor' playerX
        y = floor' playerY
    in Translate (x * scale') (y * scale')
         . Scale (scale' * if playerDirection then 1 else -1) scale'
         . BitmapSection (Rectangle (32 * playerFrame, 0) (32, 32))
         $ spritePlayer

  renderDoor x y =
    Translate (x * scale') y . Scale scale' scale' . Bitmap $ spriteDoor

  renderBlocks = Color colourFg . Pictures . fmap renderBlock

  renderBlock = Polygon . fmap (\(a, b) -> (a * scale', b * scale'))

  floor' = fromIntegral . (floor :: Float -> Int)
