{-# LANGUAGE GADTs #-}
module ColorGuess (runGameM_) where


import Control.Monad.State (StateT,runStateT)
import System.Random (newStdGen)
import Control.Monad.Prompt (Prompt,runPromptM)

import Types
import Utilities

runGameM_ :: IO ()
runGameM_ = runGameM >> return ()
  where runGameM = runStateT game =<< pickColor <$> newStdGen

game ::StateT (Secret Color) IO ()
game = (runPromptM gameInterp play)

play :: Prompt GuessPrompt ()
play = do
  action <- queryPrompt
  case action of
    (ColorIs color) -> guess color >> play 
    Hint          -> hint >> play
    NOOP          -> play
    End           -> end

gameInterp :: GuessPrompt a -> StateT (Secret Color) IO a
gameInterp (Say str)     = gPrint str
gameInterp (Query str)   = getGuess str
gameInterp (Guess color) = evalGuess color
gameInterp Quit          = return ()

