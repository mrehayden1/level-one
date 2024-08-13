module Game.Menu (
  menu
) where

import Control.Monad.Random
import Data.MemoTrie
import Data.Word
import Reflex

import Game.Env
import Game.Player

type MenuSelection = Int

menu :: Game t m => Event t () -> Event t InputEvent -> m (Dynamic t Picture)
menu eTick eInput = do
  PlayerOutput{..} <- player (-blockWidth', blockTop) FLeft eTick eInput

  let selection = ffor playerFacing $ \case
        FLeft  -> 0
        FRight -> 1

  frameNo <- foldDyn (\_ n -> succ n) (0 :: Int) eTick
  let selectionAlpha = fmap frameAlphaNorm frameNo
  render <- makeMenuRenderer
  -- Random sub-heading
  subHeading <- fmap (subHeadings !!)
    . liftIO . getRandomR $ (0, length subHeadings - 1)
  -- Random start text
  let menuPicture = render subHeading <$> selection <*> selectionAlpha
      keysVisible = pure True
  keysPicture <- keys keysVisible . fmap (\(x, y) -> (x, y + 36)) $ playerPosition
  return $ Pictures <$> sequence [menuPicture, playerPicture, keysPicture]
 where
  subHeadings = [
      "Come in. You'll never stay in another hotel again."
    ]

  frameAlphaNorm :: Int -> Word8
  frameAlphaNorm =
    -- Flashes twice a second.
    let flashPeriod' = fps' / 2
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

makeMenuRenderer :: MonadReader Env m
  => m (String -> MenuSelection -> Word8 -> Picture)
makeMenuRenderer = do
  font <- asks envFont
  sprites <- asks envSprites
  -- Create a trie that caches the render of the menus text
  let renderText = memo3 $ renderMenuText font
  return $ renderMenu renderText sprites

keys :: Game t m => Dynamic t Bool -> Dynamic t Point -> m (Dynamic t Picture)
keys visible position = do
  sprites <- asks envSprites
  let alpha = fmap (const 1) visible
  return $ renderKeys sprites <$> alpha <*> position

renderKeys :: Sprites -> Float -> Point -> Picture
renderKeys Sprites{..} alpha (x, y) =
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
        Color (withAlpha (1 - alpha) colourBg) . Polygon
          $ [(-8, -8), (24, -8), (24, 8), (-8, 8)]
      ]

-- Render the menu, with a memoised text renderer.
renderMenu :: (FontVariantI -> Word8 -> String -> Picture)
  -> Sprites
  -> String
  -> MenuSelection
  -> Word8
  -> Picture
renderMenu renderText Sprites{..} subHeading i alphaNorm =
  Pictures [
      -- Heading
      Translate (-(gw / 2)) (gh / 2 - lineHeight * 1.2)
        . Scale (1/2) (1/2)
        . renderText headingFont maxBound
        $ "No Exit: The Game",
      Translate (-(gw / 2)) (gh / 2 - lineHeight * 2.2)
        . Scale (1/2) (1/2)
        . renderText bodyFont maxBound
        $ "  " ++ subHeading,
      -- Menu items
      Pictures . fmap (uncurry h) $ items,
      -- Block
      Color colourFg . Polygon $ [
          (  0,  blockTop),
          (  0, -gh      ),
          (-gw, -gh      ),
          (-gw,  blockTop)
        ]
    ]
 where
  items = zip [0..] ["Start", "Exit"]

  h n str =
    Scale (1/4) (1/4)
      . Translate 0 (-(lineHeight * 1.5 * fromIntegral n))
      $ if i == n
          then renderText headingFont alphaNorm str
          else renderText headingFont maxBound str

  gw = gameWidth'
  gh = gameHeight'

renderMenuText :: Font -> FontVariantI -> Word8 -> String -> Picture
renderMenuText font variant alphaNorm =
  let alpha = fromIntegral alphaNorm / fromIntegral (maxBound :: Word8)
      colour = withAlpha alpha colourFg
  in createText font variant colour

blockTop :: Float
blockTop = -(gameHeight' / 2) + gameHeight' / 6
