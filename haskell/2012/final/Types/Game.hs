{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Game where

import Data.Aeson
import Data.List as List
import GHC.Generics
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict as Map

import Types.Player as Player
import Types.Tile as Tile

data GameState = Initiating | InPlay | Completed
               deriving (Show, Eq)

instance FromJSON GameState where
    parseJSON (String s) = gameState s
      where
        gameState "initiating" = pure $ Initiating
        gameState "in play" = pure $ InPlay
        gameState "completed" = pure $ Completed
        gameState _ = mzero
    parseJSON _ = mzero


data Game = Game
          { rows :: Int
          , cols :: Int
          -- tiles remaining in the draw pool
          , draw_size :: Int
          -- number of claims already prsent
          , claims :: [Claim]
          , players :: [Player]
          , player_id :: String
          , state :: GameState
          } deriving (Show, Eq, Generic)

instance FromJSON Game

playerById :: String -> [Player] -> Maybe Player
playerById pid ps = find (\p -> Player.id p == pid) ps

playerTiles :: String -> [Player] -> [Tile]
playerTiles pid ps = fromMaybe [] $ (handTiles . hand <$> playerById pid ps)

type ClaimsMap = HashMap (Row, Col) PlayerId

buildClaims :: [Claim] -> ClaimsMap
buildClaims cs = List.foldr setClaim (Map.empty) cs

claimsList :: ClaimsMap -> [Claim]
claimsList cm = List.map claimFromMap $ Map.toList cm

claimFromMap :: ((Row, Col), PlayerId) -> Claim
claimFromMap ((r, c), p) = Claim (Tile r c) (Just p)

setClaim :: Claim -> ClaimsMap -> ClaimsMap
setClaim (Claim _ Nothing) b = b
setClaim (Claim (Tile row col) (Just own)) b = Map.insert (row, col) own b
