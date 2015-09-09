{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Tile where

import Data.Aeson
import Data.Hashable
import GHC.Generics

data Tile = Tile
          { row :: Row
          , col :: Col
          } deriving (Show, Eq, Generic)

instance FromJSON Tile
instance ToJSON Tile

instance Hashable Tile where
    hashWithSalt i (Tile r c) = hashWithSalt i (r, c)
