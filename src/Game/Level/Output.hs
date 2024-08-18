module Game.Level.Output (
  Output(..)
) where

import Reflex

import Game.Env

data Output t = Output {
    outEQuit :: Event t (),
    outEChangeLevel :: Event t Int,
    outPicture :: Dynamic t Picture
  }
