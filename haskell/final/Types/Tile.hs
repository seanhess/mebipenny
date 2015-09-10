{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Tile where

import Data.Aeson
import Data.Hashable
import GHC.Generics
import Mebipenny.Board

data Tile = Tile
          { row :: Row
          , col :: Col
          } deriving (Show, Eq, Generic)

instance FromJSON Tile
instance ToJSON Tile

instance Hashable Tile where
    hashWithSalt i (Tile r c) = hashWithSalt i (r, c)

instance Loc Tile where
    row (Tile r _) = r
    col (Tile _ c) = c
    location = Tile

instance Ord Tile where
    (Tile r1 c1) <= (Tile r2 c2) = r1 <= r2 && c1 <= c2
