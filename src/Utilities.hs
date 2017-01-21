module Utilities 
  ( pickColor, colors ) where

import System.Random (RandomGen)
import System.Random.Shuffle (shuffle')

import Types (Secret (..), Color (..))


-- | Utilities for color choosing
pickColor :: RandomGen a => a -> Secret Color
pickColor gen = Secret color
  where (color: _) = shuffle' colors (length colors) gen

colors :: [Color]
colors = [Red .. Violet]
