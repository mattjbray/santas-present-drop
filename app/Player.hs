{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Player where

import           Data.Text           (Text)
import GHC.Generics
import Data.Aeson

import Position (Position)

data Player = Player
  { name :: Text
  , position :: Position
  , score :: Int
  }
  deriving (Show, Generic, FromJSON)
