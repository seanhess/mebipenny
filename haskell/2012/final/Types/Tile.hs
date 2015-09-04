{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Tile where

import Data.Aeson
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


data Claim = Claim
           { tile :: Tile
           , owner :: Maybe String
           } deriving (Show, Eq, Generic)

instance FromJSON Claim
instance ToJSON Claim
