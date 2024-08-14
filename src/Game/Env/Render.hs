module Game.Env.Render (
  scale',

  initialise,
  module Graphics.Gloss.Rendering,

  module Graphics.Gloss.Data.Color,

  Font,
  FontVariantI,
  loadFont,
  headingFont,
  bodyFont,

  createText,

  Sprites(..),
  loadSprites,

  Sprite,
  sprite,
  staticSprite,

  gameHeight,
  gameWidth,
  gameHeight',
  gameWidth',

  blockWidth,
  blockWidth',

  lineHeight,

  colourBg,
  colourFg,

  background
) where

import Codec.BMP
import Control.Monad
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Rendering (Picture(..), Point, Path)
import Graphics.Gloss.Rendering as Gloss

import Game.Env.Render.Text
import Game.Env.Window hiding (initialise)
import Util

initialise :: IO State
initialise = Gloss.initState

-- Notes on scale:
-- Our character is 26px high. At 1.5m that's 17.5 game pixels per metre.

-- Screen pixels per game pixel
-- TODO Rename to something like pixel density?
scale' :: Float
scale' = min (windowWidth'  / gameWidth' )
             (windowHeight' / gameHeight')

lineHeight :: Float
lineHeight = 32

data Sprites = Sprites {
  spritesDoor :: Sprite,
  spritesKeys :: Sprite,
  spritesPlayer :: Sprite
}

loadSprites :: IO Sprites
loadSprites =
  liftA3 Sprites
    (readSprite "sprites/Door.bmp" (0, -16) 32 32)
    (readSprite "sprites/Keys.bmp" (0, 0) 16 16)
    (readSprite "sprites/Player.bmp" (0, -16) 32 32)

readSprite :: String -> Point -> Int -> Int -> IO Sprite
readSprite filename orig width height = do
  mBitmap <- return . fmap bitmapDataOfBMP <=< readBMP $ filename
  case mBitmap of
    Left err  -> fail $ "Parse error: " ++ show err
    Right bmp -> return $ Sprite bmp orig width height

data Sprite = Sprite {
  spriteBitmap :: BitmapData,
  spriteOrigin :: Point, -- Bitmap data has an origin in the centre
  spriteWidth :: Int,
  spriteHeight :: Int
}

sprite :: Int -> Sprite -> Picture
sprite frame Sprite{..} =
  let (x, y) = spriteOrigin
  in Translate (-x) (-y)
       . BitmapSection (Rectangle (spriteWidth * frame, 0)
                                  (spriteWidth, spriteHeight))
       $ spriteBitmap

staticSprite :: Sprite -> Picture
staticSprite = sprite 0

colourBg :: Color
colourBg = makeColor (246/255) (117/255) (122/255) 1

colourFg :: Color
colourFg = makeColor (158/255) ( 40/255) ( 53/255) 1

gameHeight, gameWidth :: Int -- World pixels
gameHeight = 260 -- ~15m
gameWidth  = 420 --  24m

blockWidth :: Int -- World pixels
blockWidth = 21 -- 20 blocks across

blockWidth' :: Float
blockWidth' = fromIntegral blockWidth

gameHeight', gameWidth' :: Float
gameHeight' = fromIntegral gameHeight
gameWidth' = fromIntegral gameWidth

background :: Picture
background = Color colourBg . Polygon $ [
    (gameWidth', gameHeight'),
    (gameWidth', -gameHeight'),
    (-gameWidth', -gameHeight'),
    (-gameWidth', gameHeight')
  ]
