module Solve where

import Data.Vector as Vector
import Data.List as List
import Types.Tile as Tile
import Types.Game as Game
import Types.Player as Player
import Game as Game
import AIExpectimax (Probability, Distribution, Utility)

data TurnState = TurnState
  { board :: Board
  , game :: Game
  , tiles :: [Tile]
  } deriving (Show, Eq)

nextMoves :: TurnState -> [TurnState]
nextMoves s = List.map (placeMove s) (tiles s)

placeMove :: TurnState -> Tile -> TurnState
placeMove s t =
  let b' = setTile (Just (player_id . game $ s)) t (board s)
      ts' = List.filter (/= t) (tiles s)
  in s { board = b', tiles = ts' }

-- TODO needs to handle N players at some point
-- more than one tile placed
moveDistribution :: TurnState -> Distribution TurnState
moveDistribution grid = undefined

-- my score! yay!
-- now, I can get fancier than this
-- like if there are no draws left, and I win... yeehawww...
-- you know, I really want the whole game
-- it's the number of claims I have
stateUtility :: TurnState -> Utility
stateUtility s = undefined

