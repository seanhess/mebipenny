{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Move where

import Types.Tile
import Data.Aeson
import GHC.Generics

data NewMove = Pass | NewMove Move

instance ToJSON NewMove where
    toJSON Pass = String "PASS"
    toJSON (NewMove m) = toJSON m

data Move = Move
          { tile :: Tile
          , favor :: String
          } deriving (Show, Eq, Generic)

instance ToJSON Move
instance FromJSON Move
