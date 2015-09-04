module Engine where

-- I need to be able to predict the optimal move
import Types.Tile
import Types.Game as Game
import Types.Player

import Data.Maybe as Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List as List
import Data.Function (on)

data Board = Board
  { rows :: Rows
  , cols :: Cols
  , claimsMap :: ClaimsMap
  } deriving (Show, Eq)

takenClaims :: ClaimsMap -> [Claim]
takenClaims b = map toClaim $ HM.toList b
  where
    toClaim ((r, c), p) = Claim (Tile r c) (Just p)

gameBoard :: Game -> Board
gameBoard game = board (Game.rows game) (Game.cols game) (Game.claims game)

board :: Rows -> Cols -> [Claim] -> Board
board rows cols cs = Board rows cols $ buildClaims cs

claim :: Board -> Tile -> Maybe Claim
claim board (Tile r c) =
    let mp = HM.lookup (r, c) (claimsMap board)
        tile = Tile r c
    in
    if validTile board tile
      then Just $ Claim tile mp
      else Nothing

hasOwner :: Claim -> Bool
hasOwner claim = isJust (owner claim)

bestMove :: PlayerId -> [Tile] -> Board -> Maybe (Int, Tile)
bestMove _ [] _ = Nothing
bestMove p ts b =
    let boards = map (placeTile p b) ts
        scores = map (playerScore p . takenClaims . claimsMap) boards
        both = zip scores ts
        best = maximumBy (compare `on` fst) both
    in Just best

placeTile :: PlayerId -> Board -> Tile -> Board
placeTile p b t =
    let army = neighborArmy p t b
        winner = winningPlayer p army
    in foldr (conquerClaim winner) b army

winningPlayer :: PlayerId -> [Claim] -> PlayerId
winningPlayer pid [] = pid
winningPlayer pid cs =
    let owners = nub $ catMaybes $ map owner cs
        scores = map (\p -> playerScore p cs) owners
        both = zip owners scores :: [(PlayerId, Int)]
        max = maximumBy (compare `on` snd) both
        pScore = fromMaybe 0 $ lookup pid both
    in win (pid, pScore) max
  where
    win (p, ps) (o, os)
      | ps > os  = p
      | ps == os = p
      | ps < os  = o

-- returns the elements!
playerScore :: PlayerId -> [Claim] -> Int
playerScore p cs = length $ filter (== p) $ catMaybes $ map owner cs

conquerClaim :: PlayerId -> Claim -> Board -> Board
conquerClaim p c b =
    let cmap = setClaim (c { owner = Just p }) (claimsMap b)
    in b { claimsMap = cmap }

-- returns the addresses of nearest neighbors?
-- nearest guys, limited to stuff that's actually there
neighbors :: Tile -> [Tile]
neighbors (Tile r c) =
    [                   Tile (r+1) (c)
    , Tile (r)   (c-1),                 Tile (r)   (c+1)
    ,                   Tile (r-1) (c)
    ]

validTile :: Board -> Tile -> Bool
validTile (Board rows cols _) (Tile r c) = r >= 0 && c >= 0 && r < rows && c < cols

neighborArmy :: PlayerId -> Tile -> Board -> [Claim]
neighborArmy playerId tile board =
  let ts = neighbors tile
      new = Claim tile (Just playerId)
  in new : List.filter hasOwner (mapMaybe (claim board) ts)

----------------------------------------------------------

sampleBoard :: Board
sampleBoard = board 2 2 sampleClaims

sampleClaims :: [Claim]
sampleClaims = [Claim (Tile 0 1) (Just "bob")]
