module Utilities (pickColor) where

import System.Random
import System.Random.Shuffle

import Types

pickColor :: RandomGen a => a -> Secret Color
pickColor gen = Secret color
  where (color: _) = shuffle' colors (length colors) gen

colors :: [Color]
colors = [Red .. Blue]
