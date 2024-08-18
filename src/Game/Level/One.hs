module Game.Level.One (
  level
) where

import Control.Monad
import Reflex

import Game.Block hiding (render)
import qualified Game.Block as Block
import qualified Game.Door as Door
import Game.Env
import Game.Level.Output
import Game.Linear
import qualified Game.Player as Player
import Game.Player hiding (Output)

level :: forall t m. Game t m => Event t Float -> m (Output t)
level eTick = do
  sprites <- asks envSprites

  playerOut <- player playerStart FRight blocks eTick

  let entrancePicture = uncurry Translate entrance . Door.render $ sprites
      exitPicture = uncurry Translate exit . Door.render $ sprites
      blocksPicture  = Pictures . fmap Block.render $ blocks

  let picture = fmap Pictures . sequence $ [
                    pure entrancePicture,
                    pure exitPicture,
                    pure blocksPicture,
                    Player.outPicture playerOut
                  ]

  return $ Output never never picture

playerStart :: P2 Float
playerStart = P (V2 (gridCoord (-8)) (gridCoord (-1)))

entrance :: Point
entrance = (gridCoord (-8), gridCoord (-1))

exit :: Point
exit = (gridCoord 9, gridCoord (-1))

blocks :: [Block]
blocks = do
  x <- [-10..10]
  y <- [-6..6]
  -- one block
  --guard $ y == -1 && x == 0
  -- two blocks
  --guard $ y == -1 && (x == 0 || x == 1)
  -- two blocks right angle
  --guard $ y == -1 && x == 0 || y == 0 && x == 1
  -- three blocks right angle
  --guard $ y == -1 && (x == 0 || x == -1)
  -- four blocks right angle
  --guard $ y == -1 && x > -2 && x < 2 || x == 1 && y == 0
  -- corridoor
  guard $ notElem y [-1, 0, 1] || elem x [-10, 10]
  return . makeBlock $ (x, y)
 where
  makeBlock :: (Int, Int) -> Block
  makeBlock (x, y) =
    let ox = gridCoord x
        oy = gridCoord y
    in Block (ox, oy) Block.size' Block.size'
