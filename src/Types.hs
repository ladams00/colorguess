{-# LANGUAGE GADTs #-}
module Types 
  ( Action (..)
  , Result (..)
  , Secret (..)
  , Color (..)
  , GuessPrompt (..)
  ) where

data Action = Guess Color | Hint | NOOP | End deriving (Read,Show)

data Result = Colder | Correct | Warmer deriving (Enum,Show)

newtype Secret a = Secret { fromSecret :: a }

data Color 
  = Red 
  | Orange 
  | Yellow 
  | Green 
  | Blue 
     deriving (Enum, Eq, Ord,Read, Show)

data GuessPrompt a where
  Say      :: String -> GuessPrompt ()
  Query    :: String -> GuessPrompt Action
  Response :: Color -> GuessPrompt Result
  Quit     :: GuessPrompt ()
