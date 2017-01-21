module Utilities 
  ( pickColor
  , colors
  , queryPrompt
  , guess
  , hint
  , end
  , evalGuess
  , gPrint
  , getGuess 
  ) where

import Safe (readDef)
import System.Random (RandomGen)
import System.Random.Shuffle (shuffle')
import Control.Monad.State (StateT,lift,get)

import Lens.Micro ((<&>))
import Control.Monad.Prompt (Prompt,prompt)
import Data.Monoid ((<>))
import Types

-- | Prompts
queryPrompt :: Prompt GuessPrompt Action
queryPrompt = prompt (Say commands) >> prompt (Query guess')
  where
    guess'    = "Make a guess. Colors are " <> (concatMap show colors)
    commands = "Commands are <ColorIs color> <Hint> <End>"

guess :: Color -> Prompt GuessPrompt ()
guess color = prompt (Guess color) >>= fromResult

hint :: Prompt GuessPrompt ()
hint = prompt (Say "a hint, really?") 

end :: Prompt GuessPrompt () 
end = prompt Quit

-- | For the interpreter
evalGuess :: Color -> StateT (Secret Color) IO Result
evalGuess color = toEnum <$> fromEnum <$> compare color <$> fromSecret <$> get

gPrint :: String -> StateT (Secret Color) IO ()
gPrint str = lift (putStrLn str)

getGuess :: String -> StateT (Secret Color) IO Action
getGuess str = lift (putStrLn str) >> lift getLine <&> readDef NOOP


-- | Utilities for color choosing
pickColor :: RandomGen a => a -> Secret Color
pickColor gen = Secret color
  where (color: _) = shuffle' colors (length colors) gen

colors :: [Color]
colors = [Red .. Blue]

-- | Internal
fromResult :: Result -> Prompt GuessPrompt ()
fromResult Correct = prompt (Say "you are correct sir!") >> end
fromResult res     = prompt (Say (show res)) 


