{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Position where

import GHC.Generics
import Data.Aeson

data Position = Position
  { x :: Double
  , y :: Double
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)
