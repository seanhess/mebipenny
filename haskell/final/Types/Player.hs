{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Player where

import Control.Monad
import Data.Aeson
import GHC.Generics
import Data.Text

import Types.Tile

type PlayerId = String

data Score = Score Integer | Disqualified
           deriving (Show, Eq)

instance FromJSON Score where
    parseJSON (Number n) = pure $ Score (round n)
    parseJSON (String "disqualified") = pure Disqualified
    parseJSON _ = pure Disqualified

instance ToJSON Score where
    toJSON (Score n) = Number $ fromInteger n
    toJSON (Disqualified) = String "disqualified"

--------------------------------------------------------------------

data Hand = Tiles [Tile] | Hand Int
          deriving (Show, Eq)

instance FromJSON Hand where
    parseJSON n@(Number _) = Hand <$> parseJSON n
    parseJSON a@(Array _) = Tiles <$> parseJSON a
    parseJSON _ = mzero

data Player = Player
            { id :: String
            , name :: String
            , score :: Score
            , hand :: Hand
            } deriving (Show, Eq, Generic)

instance FromJSON Player

handTiles :: Hand -> [Tile]
handTiles (Tiles ts) = ts
handTiles _ = []
