{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.GameDelta where

import Data.Aeson
import GHC.Generics
import Data.List as List

import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Types.Player as Player
import Types.Tile
import Types.Game (Game, GameState)
import qualified Types.Game as Game
import Types.Claim

data GameDelta = GameDelta
          { draw_size :: Int
          , claims :: [Claim]
          , players :: [Player]
          , state :: GameState
          } deriving (Show, Eq, Generic)

instance FromJSON GameDelta

applyDelta :: GameDelta -> Game -> Game
applyDelta delta game = game
  { Game.draw_size = draw_size delta
  , Game.claims = addClaims (Game.claims game) (claims delta)
  , Game.players = addPlayers (Game.players game) (players delta)
  , Game.state = state delta
  }

addClaims :: [Claim] -> [Claim] -> [Claim]
addClaims old new = nubBy cmp (new ++ old)
  where
    cmp c1 c2 = tile c1 == tile c2

addPlayers :: [Player] -> [Player] -> [Player]
addPlayers old new = nubBy cmp (new ++ old)
  where
    cmp p1 p2 = Player.id p1 == Player.id p2

