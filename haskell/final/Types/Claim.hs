{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Claim where

import Data.Aeson
import Data.List as List
import Data.Maybe as Maybe
import GHC.Generics
import Types.Tile as Tile
import Types.Player as Player

-- but IS this the right representation?
-- Yes, it's a claim
-- the other one is different
-- the other one is a maybe claim :)
-- well they are both valid
data Claim = Claim
           { tile :: Tile
           , owner :: Maybe PlayerId
           } deriving (Show, Eq, Generic)

data TakenClaim = TakenClaim Tile PlayerId
                deriving (Show, Eq)

toTakenClaim :: Claim -> Maybe TakenClaim
toTakenClaim (Claim tile Nothing) = Nothing
toTakenClaim (Claim tile (Just p)) = Just $ TakenClaim tile p

toTakenClaims :: [Claim] -> [TakenClaim]
toTakenClaims = mapMaybe toTakenClaim

instance FromJSON Claim
instance ToJSON Claim
