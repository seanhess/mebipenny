{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.GameDelta where

import Data.Aeson
import GHC.Generics
import Data.List as List

import Types.Player
import Types.Tile
import Types.Game (Game, GameState, buildClaims, setClaim, claimsList)
import qualified Types.Game as Game

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
  , Game.players = players delta
  , Game.state = state delta
  }

addClaims :: [Claim] -> [Claim] -> [Claim]
addClaims old new =
    let cmap = buildClaims old
    in claimsList $ List.foldr setClaim cmap new

