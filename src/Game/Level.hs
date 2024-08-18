module Game.Level (
  Output(..),

  one
) where

import Reflex

import Game.Env
import Game.Level.One
import Game.Level.Output

one :: Game t m => Event t Float -> m (Output t)
one = Game.Level.One.level
