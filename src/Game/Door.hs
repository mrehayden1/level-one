module Game.Door (
  render
) where

import Game.Env

render :: Sprites -> Picture
render Sprites{..} = Translate 0 (-1) . staticSprite $ spritesDoor
