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
import Types.Claim as Claim

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
          , drawSize :: Int
          -- claims made specifically, not missing
          , claims :: [Claim]
          , players :: [Player]
          , playerId :: String
          , state :: GameState
          } deriving (Show, Eq, Generic)

-- instance FromJSON Game where
    -- parseJSON (Object obj) = do
      -- Game <$>
      -- obj .: "rows" <*>
      -- obj .: "cols" <*>
      -- obj .: "draw_size" <*>
      -- obj .: "claims" <*>

playerById :: String -> [Player] -> Maybe Player
playerById pid ps = find (\p -> Player.id p == pid) ps

playerTiles :: String -> [Player] -> [Tile]
playerTiles pid ps = fromMaybe [] $ (handTiles . hand <$> playerById pid ps)

