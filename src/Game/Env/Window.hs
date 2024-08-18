module Game.Env.Window (
  Window(..),
  windowHeight,
  windowWidth,
  windowHeight',
  windowWidth',

  initialise,
  closeWindow,

  displayPicture,
  GLFW.pollEvents
) where

import Control.Monad
import Data.Maybe
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Rendering hiding (displayPicture)
import qualified Graphics.Gloss.Rendering as Gloss
import qualified Graphics.UI.GLFW as GLFW

windowHeight, windowWidth :: Int
--windowHeight = 1440
--windowWidth = 2560
windowHeight = 1080
windowWidth = 1920

windowHeight', windowWidth' :: Float
windowHeight' = fromIntegral windowHeight
windowWidth' = fromIntegral windowWidth

fullscreen :: Bool
--fullscreen = False
fullscreen = True

vsyncEnabled :: Bool
vsyncEnabled = True

data Window = Window {
  glfwWindow :: GLFW.Window,
  glossState :: Gloss.State
}

initialise :: String -> IO Window
initialise title = Window <$> initialiseWindow title <*> Gloss.initState

initialiseWindow :: String -> IO GLFW.Window
initialiseWindow title = do
  r <- GLFW.init
  unless r (error "GLFW.init failed.")
  GLFW.defaultWindowHints
  -- Stop user resizing window
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  -- Create window
  monitor' <- if fullscreen then GLFW.getPrimaryMonitor else return Nothing
  window <- fmap (fromMaybe (error "GLFW failed to create window."))
    . GLFW.createWindow windowWidth windowHeight title monitor'
    $ Nothing
  GLFW.makeContextCurrent (Just window)
  -- Hide cursor
  GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
  -- Capture raw mouse motion if supported
  rawMouseSupported <- GLFW.rawMouseMotionSupported
  if rawMouseSupported
    then
      GLFW.setRawMouseMotion window True
    else
      putStrLn "Raw mouse motion unsupported."
  -- Vsync
  GLFW.swapInterval $ if vsyncEnabled then 1 else 0
  return window

closeWindow :: GLFW.Window -> IO ()
closeWindow w = do
  GLFW.setWindowShouldClose w True
  GLFW.destroyWindow w
  GLFW.terminate

displayPicture :: Window -> Picture -> IO ()
displayPicture w p = do
  Gloss.displayPicture (windowWidth, windowHeight) white (glossState w) 1 p
  GLFW.swapBuffers . glfwWindow $ w
