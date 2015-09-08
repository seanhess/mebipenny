{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Tile where

import Data.Aeson
import Data.Hashable
import GHC.Generics

type Col = Int
type Row = Int
type Rows = Int
type Cols = Int

data Tile = Tile
          { row :: Row
          , col :: Col
          } deriving (Show, Eq, Generic)

instance FromJSON Tile
instance ToJSON Tile

instance Hashable Tile where
    hashWithSalt i (Tile r c) = hashWithSalt i (r, c)
