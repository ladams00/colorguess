{-# LANGUAGE GADTs #-}
module ColorGuess (runGameM_) where

import Safe

import Control.Monad.State
import System.Random (newStdGen)
import Lens.Micro ((<&>))
-- import Data.Monoid ((<>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Prompt

import Types
import Utilities

runGameM_ :: IO ()
runGameM_ = runGameM >> return ()
  where runGameM = runStateT game =<< pickColor <$> newStdGen

game ::StateT (Secret Color) IO ()
game = (runPromptM gameInterp play)

play :: Prompt GuessPrompt ()
play = do
  action <- prompt (Query "make a guess")
  case action of
    (Guess color) -> prompt (Response color) >>= response 
    Hint          -> prompt (Say "a hint, really?") >> play
    NOOP          -> play
    End           -> prompt (Say "Game over, Man")

gameInterp :: GuessPrompt a -> StateT (Secret Color) IO a
gameInterp (Say x) = lift (putStrLn x)
gameInterp (Query x) = 
  lift (putStrLn x) >> lift getLine <&> readDef NOOP 
gameInterp (Response guess) = 
  toEnum <$> fromEnum <$> compare guess <$> fromSecret <$> get
gameInterp Quit = return ()
    
response :: Result -> Prompt GuessPrompt ()
response Correct = prompt (Say "you are correct sir!") >> end
response res = prompt (Say (show res)) >> play

end :: Prompt GuessPrompt () 
end = prompt Quit

