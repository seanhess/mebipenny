module Solve where

import Data.List as List
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Types.Tile as Tile
import Types.Claim as Claim
import qualified Types.Game as Game
import Types.Game (GameState, Game)
import Types.Player as Player
-- import qualified Game as Game
import Mebipenny.AIExpectimax
import Mebipenny.Board as Board
import Mebipenny.Functions
import Debug.Trace
import Data.Function (on)
-- import qualified Data.Set as Set
-- import Data.Set (Set)

-- type Army = HashMap Tile Piece
type Army = [(Tile, PlayerId)]
-- type Armies = HashMap ArmyName Army
type Armies = [Army]
type ArmyName = (PlayerId, Int)

-- I should keep a whole log of what's goin on here!
data TurnState = TurnState
  { board :: Board Tile Piece
  , tiles :: [Tile]
  , drawSize :: Int

  -- all players? no that doesn't really matter
  , players :: [PlayerId]

  -- my playerId
  , playerId :: String


  -- whether we've ended or not
  , state :: GameState

  , choices :: [Tile]
  } deriving (Show, Eq)

setBoard :: Board Tile Piece -> TurnState -> TurnState
setBoard b s = s { board = b }

setTiles :: [Tile] -> TurnState -> TurnState
setTiles ts s = s { tiles = ts }

setDrawSize :: Int -> TurnState -> TurnState
setDrawSize n s = s { drawSize = n }

gameTurnState :: Game -> TurnState
gameTurnState g =
  let p = Game.player_id g
  in TurnState
    (claimsBoard (Game.rows g) (Game.cols g) (Game.claims g))
    (Game.playerTiles p $ Game.players g)
    (Game.draw_size g)
    (List.map Player.id $ Game.players g)
    (p)
    (Game.state g)
    []

claimsBoard :: Rows -> Cols -> [Claim] -> Board Tile Piece
claimsBoard rows cols cs = Board.fromList rows cols $ List.map fromClaim cs

----------------------------------------------------------------

-- utility :: a -> Utility
-- distribution :: a -> Distribution a    ==    a -> [(Probability a)]
-- nextMoves :: a -> [a]
-- hasMoves :: a -> Bool

-- this is on an inner loop :(
-- so it's super slow
-- I mean, I could have been keeping track of armies as I go along?
-- heh... or maybe the equality test is slow, pretty likely
stateUtility :: TurnState -> Utility
stateUtility s =
  let as = armies (board s)
      scores = map (playerScore as) (players s)
  in sum $ map eachScore $ zip (players s) scores

  where
    eachScore (p, score)
      | p == (playerId s) = score
      | otherwise = -score

armyScore :: Army -> Utility
armyScore a = fromIntegral (length a) ** 2

armyOwner :: Army -> PlayerId
armyOwner ((_,p):as) = p
armyOwner _ = "?"

playerScore :: [Army] -> PlayerId -> Utility
playerScore as p = sum $ map armyScore $ filter ((== p) . armyOwner) as

-- TODO: simple algorithm and have them fight
-- just count the number of squares!
simpleScore :: Board Tile Piece -> PlayerId -> Utility
simpleScore b p = fromIntegral $ length $ filter ((==(Taken p)) . snd) $ Board.toList b



-- TODO all opponents
moveDistribution :: TurnState -> Distribution TurnState
moveDistribution s =
  -- it should call this with my state and it chosen already
  let ps = players s
      p = playerId s
      opponents = filter (/= p) ps
  in moveDistributionOpponent s (head opponents)

moveDistributionOpponent :: TurnState -> PlayerId -> Distribution TurnState
moveDistributionOpponent s op =
  let b = board s
      p = playerId s
      empty = List.filter (isEmpty . snd) $ Board.toList b
      tot = fromIntegral (length empty)
      prob = 1.0 / tot
      states = List.map (placeTile s op . fst) empty
  in List.map (\s -> (prob, s)) states

nextMoves :: TurnState -> [TurnState]
nextMoves s = List.map (placeTile s (playerId s)) (tiles s)

hasMoves :: TurnState -> Bool
hasMoves s = length (tiles s) > 0

--------------------------------------------------------------------

-- with pruning...
armies :: Board Tile Piece -> Armies
armies b = filter (not . List.null) $ foldr go [] $ Board.allLocations b
  where

    go :: Tile -> Armies -> Armies
    go tile armies
      | any (isInArmy tile) armies = armies
      | otherwise = findArmy b [] tile : armies

    isInArmy :: Tile -> Army -> Bool
    isInArmy tile army = isJust $ List.lookup tile army


--------------------------------------------------------------------

depth = 2

bestMove :: Int -> TurnState -> [Tile]
bestMove d s =
  let tree = buildEXTree hasMoves nextMoves moveDistribution d s
  in choices $ bestChoice stateUtility tree

---------------------------------------------------------------------

placeTile :: TurnState -> PlayerId -> Tile -> TurnState
placeTile s pid t =
  let b = board s
      myArmy = [(t, pid)]
      allArmies = myArmy : findNeighboringArmies b t
      w = winner (playerId s) allArmies
      allTiles = map fst $ concat allArmies :: [Tile]
      b' = conquerTiles w allTiles b
      ts' = List.delete t (tiles s)
  in s { board = b', tiles = ts', choices = t:(choices s) }

winner :: PlayerId -> [Army] -> PlayerId
winner pid [] = pid
winner pid as =
  let scores = mapMaybe score as :: [(PlayerId, Int)]
      max = maximumBy (compare `on` snd) scores :: (PlayerId, Int)
      pScore = fromMaybe 0 $ List.lookup pid scores
  in win (pid, pScore) max
  where
    win (p, ps) (o, os)
      | ps >= os = p
      | ps < os  = o

    score :: Army -> Maybe (PlayerId, Int)
    score ((t,p):as) = Just (p, length as + 1)
    score _ = Nothing

conquerTiles :: PlayerId -> [Tile] -> Board Tile Piece -> Board Tile Piece
conquerTiles p ts b = List.foldr (conquerTile p) b ts

conquerTile :: PlayerId -> Tile -> Board Tile Piece -> Board Tile Piece
conquerTile p t b = Board.setPiece b t (Taken p)

-- still not including my piece!
findNeighboringArmies :: Board Tile Piece -> Tile -> [Army]
findNeighboringArmies b t = filter (not . List.null) $ map (findArmy b []) (adjacent b t)

findArmy :: Board Tile Piece -> Army -> Tile -> Army
findArmy b visited tile =
  case Board.lookup b tile of
    Taken p ->
      let visited' = (tile, p) : visited
          us = unique visited (adjacent b tile)
      in
      -- this is the normal iteration, but we pass a separate visited thing in
      (tile,p) : List.concatMap (findArmy b visited') us
    _ -> []
  where
    unique army tiles = filter (isNothing . flip List.lookup army) tiles

---------------------------------------------------------------------

sampleBoard :: Board Tile Piece
sampleBoard = Board.fromList 6 6
  [ (Tile 0 1, Wall)
  , (Tile 0 3, Taken "bob")
  , (Tile 0 2, Taken "bob")
  , (Tile 0 4, Taken "bob")
  , (Tile 2 3, Taken "alice")
  , (Tile 2 2, Taken "alice")
  , (Tile 3 2, Taken "alice")
  , (Tile 4 2, Taken "alice")
  ]

sampleTurnState :: TurnState
sampleTurnState = TurnState
  (sampleBoard)
  ([Tile 3 3, Tile 5 5, Tile 1 3])
  (20)
  (["alice", "bob"])
  ("alice")
  (Game.InPlay)
  []

