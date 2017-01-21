{-# LANGUAGE GADTs #-}
module Types 
  ( Action (..)
  , Result (..)
  , Secret (..)
  , Color (..)
  , GuessPrompt (..)
  ) where

data Action = ColorIs Color | Hint | NOOP | End deriving (Read,Show)

data Result = Colder | Correct | Warmer deriving (Enum,Show)

newtype Secret a = Secret { fromSecret :: a }

data Color 
  = Red 
  | Orange 
  | Yellow 
  | Green 
  | Blue 
  | Violet
     deriving (Enum, Eq, Ord,Read, Show)

data GuessPrompt a where
  Say   :: String -> GuessPrompt ()
  Query :: String -> GuessPrompt Action
  Guess :: Color -> GuessPrompt Result
  Quit  :: GuessPrompt ()
