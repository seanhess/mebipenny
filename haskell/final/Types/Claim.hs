{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Claim where

import Data.Aeson
import Data.List as List
import Data.Maybe as Maybe
import GHC.Generics
import Types.Tile as Tile
import Types.Player as Player
import Mebipenny.Board (BoardPiece(..))

-- but IS this the right representation?
-- Yes, it's a claim
-- the other one is different
-- the other one is a maybe claim :)
-- well they are both valid
data Claim = Claim
           { tile :: Tile
           , owner :: Maybe PlayerId
           } deriving (Show, Eq, Generic)

instance FromJSON Claim
instance ToJSON Claim


data Piece = Empty | Wall | Taken PlayerId
           deriving (Show, Eq, Generic)

instance BoardPiece Piece where
  unoccupied = Empty
  showc Empty = '_'
  showc Wall  = '█'
  showc (Taken [p]) = '?'
  showc (Taken (p:ps)) = p

fromClaim :: Claim -> (Tile, Piece)
fromClaim (Claim tile Nothing) = (tile, Wall)
fromClaim (Claim tile (Just p)) = (tile, Taken p)

isOwner :: PlayerId -> Piece -> Bool
isOwner p1 (Taken p2) = p1 == p2
isOwner _ _ = False

isTaken :: Piece -> Bool
isTaken (Taken _) = True
isTaken _ = False

isEmpty :: Piece -> Bool
isEmpty Empty = True
isEmpty _ = False

pieceOwner :: Piece -> Maybe PlayerId
pieceOwner (Taken p) = Just p
pieceOwner _ = Nothing
