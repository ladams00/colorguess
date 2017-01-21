{-# LANGUAGE GADTs #-}
module Interpreter ( gameInterp ) where

import Safe (readDef)
import Control.Monad.State (StateT,lift,get)
import Lens.Micro ((<&>))
import Types

gameInterp :: GuessPrompt a -> StateT (Secret Color) IO a
gameInterp (Say str)     = gPrint str
gameInterp (Query str)   = getGuess str
gameInterp (Guess color) = evalGuess color
gameInterp Quit          = return ()

gPrint :: String -> StateT (Secret Color) IO ()
gPrint str = lift (putStrLn str)

getGuess :: String -> StateT (Secret Color) IO Action
getGuess str = lift (putStrLn str) >> lift getLine <&> readDef NOOP

evalGuess :: Color -> StateT (Secret Color) IO Result
evalGuess color = toEnum <$> fromEnum <$> compare color <$> fromSecret <$> get
