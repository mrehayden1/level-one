module Game.Menu (
  menu,

  Output(..)
) where

import Control.Lens
import Control.Monad.Random
import Data.Bool
import Data.MemoTrie
import Data.Word
import Linear
import Linear.Affine
import Reflex
import Reflex.Network

import Game.Block hiding (render)
import qualified Game.Block as Block
import qualified Game.Door as Door
import Game.Env hiding (Point)
import Game.Player hiding (Output)
import qualified Game.Player as Player

type MenuSelection = Int

data Output t = Output {
    outEQuit :: Event t (),
    outEStart :: Event t (),
    outPicture :: Dynamic t Picture
  }

menu :: forall t m . Game t m
  => Event t Float
  -> m (Output t)
menu eTick = do
  playerOut <- player (P (V2 (-Block.size') blockTop)) FLeft [block] eTick

  -- Menu selection
  let selection = ffor (Player.outFacing playerOut) $ \case
        FLeft  -> 0
        FRight -> 1
  -- Make it flash
  frameNo <- foldDyn (\_ n -> succ n) (0 :: Int) eTick
  let selectionAlpha = fmap frameAlphaNorm frameNo

  -- Memoize the text rendered
  render' <- makeMenuRenderer

  -- Text
  -- Random sub-heading
  subHeading <- fmap (subHeadings !!)
    . liftIO . getRandomR $ (0, length subHeadings - 1)
  let textPicture = render' subHeading <$> selection <*> selectionAlpha

  -- Keyboard keys helper prompt
  isMoving <- holdUniqDyn . fmap (not . nearZero . (^. _x))
                . Player.outVelocity $ playerOut
  rec
    keysVisible <- pure . join
                     <=< networkHold (keyPromptVisible False False)
                           . fmap (uncurry keyPromptVisible)
                     $ (,) <$> current keysVisible <@> updated isMoving
  keysPicture <- keys keysVisible . fmap (set _y (-48)) . outPosition $ playerOut

  -- Start the game when the player hits the door
  let eStart = void . ffilter ((< 1) . abs . subtract doorX . (^. _x))
        . updated . Player.outPosition $ playerOut

  -- Quit when the player walks off the edge
  let eQuit = void . ffilter ((< -200) . (^. _y)) . updated
        . Player.outPosition $ playerOut

  -- Output
  let picture = Pictures <$> sequence [
                                 textPicture,
                                 Player.outPicture playerOut,
                                 keysPicture
                               ]
  return $ Output eQuit eStart picture
 where
  subHeadings = [
      ""
    ]

  frameAlphaNorm :: Int -> Word8
  frameAlphaNorm =
    -- Flashes twice a second.
    let flashPeriod' = 12 -- hack since we have a fixed fps!
        flashPeriod  = round flashPeriod'
    -- We quantise the menu selection alpha for memoisation
    in round . (* 255) . triangleWave . (/ flashPeriod') . fromIntegral
         . (`mod` flashPeriod)

  -- Triangle wave function which peaks at whole numbers.
  triangleWave :: Float -> Float
  triangleWave t | odd (floor s :: Int) = r
                 | otherwise            = 1 - r
   where
     r = s - fromIntegral (floor s :: Int)
     s = t * 2

  -- Creates a memoised version of the text renderer and passes it to the
  -- render function.
  makeMenuRenderer :: m (String -> MenuSelection -> Word8 -> Picture)
  makeMenuRenderer = do
    font <- asks envFont
    sprites <- asks envSprites
    -- Create a trie that caches the render of the menus text
    let renderText' = memo3 $ renderText font
    return $ render renderText' sprites

  keyPromptVisible :: Bool -> Bool -> m (Dynamic t Bool)
  keyPromptVisible visible playerMoving =
    if visible /= playerMoving
      then return $ constDyn visible
      else do
        pb <- getPostBuild
        e <- delay (bool 3 0 visible) . (not visible <$) $ pb
        holdDyn visible e

keys :: Game t m
  => Dynamic t Bool
  -> Dynamic t (Point V2 Float)
  -> m (Dynamic t Picture)
keys visible position = do
  sprites <- asks envSprites
  let alpha = fmap (bool 0 1) visible
  return $ renderKeys sprites <$> alpha <*> position

renderKeys :: Sprites -> Float -> Point V2 Float -> Picture
renderKeys Sprites{..} alpha (P (V2 x y)) =
  Translate (x - 8) y
    . Pictures
    $ [
        -- Left key
        sprite 2 spritesKeys,
        -- Right key
        Translate 16 0
          . sprite 0
          $ spritesKeys,
        -- Overlay, for pseudo-transparency
        Color (withAlpha (1 - alpha) bgColour) . Polygon
          $ [(-8, -8), (24, -8), (24, 8), (-8, 8)]
      ]

-- Render the menu, with a memoised text renderer.
render :: (FontVariantI -> Word8 -> String -> Picture)
  -> Sprites
  -> String
  -> MenuSelection
  -> Word8
  -> Picture
render renderText' sprites subHeading i alphaNorm =
  Pictures [
      -- Heading
      Translate (-(gameWidth' / 2)) (gameHeight' / 2 - lineHeight * 1.2)
        . Scale (1/2) (1/2)
        . renderText' headingFont maxBound
        $ "No Exit: The Game",
      Translate (-(gameWidth' / 2)) (gameHeight' / 2 - lineHeight * 2.2)
        . Scale (1/2) (1/2)
        . renderText' bodyFont maxBound
        $ "  " ++ subHeading,
      -- Menu items
      Pictures . fmap (uncurry h) $ items,
      -- Door
      Translate doorX blockTop . Door.render $ sprites,
      -- Block
      Block.render block
    ]
 where
  items = zip [0..] ["Start", "Exit"]

  h n str =
    Scale (1/4) (1/4)
      . Translate 0 (-(lineHeight * 1.5 * fromIntegral n))
      $ if i == n
          then renderText' headingFont alphaNorm str
          else renderText' headingFont maxBound str

renderText :: Font -> FontVariantI -> Word8 -> String -> Picture
renderText font variant alphaNorm =
  let alpha = fromIntegral alphaNorm / fromIntegral (maxBound :: Word8)
      colour = withAlpha alpha fgColour
  in createText font variant colour

block :: Block
block = Block (ox, oy) (-ox) (blockTop - oy)
 where
  ox = -gameWidth'
  oy = -gameHeight'

blockTop :: Float
blockTop = -(gameHeight' / 2) + gameHeight' / 6

doorX :: Float
doorX = -50
