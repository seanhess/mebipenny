module Board where

-- I need to be able to predict the optimal move
import Types.Tile
import Types.Game as Game
import Types.Player
import Types.Claim as Claim

import Data.HashMap.Strict as Map
import Data.Maybe as Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List as List
import Data.Function (on)
import Data.List.Split as List

-- if this were a grid, we could... you know..
data Board = Board
  { rows :: Rows
  , cols :: Cols
  , grid :: HashMap Tile PlayerId
  } deriving (Show, Eq)

setGrid :: HashMap Tile PlayerId -> Board -> Board
setGrid g b = b { grid = g }

------------------------------------------------------------------
-- would it be easier to use a grid?
-- maybe I could index by anything? No, that doesn't make sense

buildBoard :: Rows -> Cols -> [Claim] -> Board
buildBoard rows cols cs = List.foldr setClaim (emptyBoard rows cols) cs

emptyBoard :: Rows -> Cols -> Board
emptyBoard rows cols = Board rows cols HM.empty

allClaims :: Board -> [Claim]
allClaims board = List.map (flip lookupTile board) (allTiles board)

takenClaims :: Board -> [TakenClaim]
takenClaims (Board _ _ grid) = List.zipWith TakenClaim (Map.keys grid) (Map.elems grid)

lookupTile :: Tile -> Board -> Claim
lookupTile t b = Claim t (Map.lookup t (grid b))

-- allTilesRows :: Board -> [[Tile]]
-- allTilesRows b = groupBy tileEq allTiles
  -- where
    -- tileEq (Tile r1 _) (Tile r2 _) = r1 == r2

allTiles :: Board -> [Tile]
allTiles (Board rows cols _) = do
    r <- [0..rows-1]
    c <- [0..cols-1]
    return $ Tile r c

-- buildClaims :: [Claim] -> ClaimsMap
-- buildClaims cs = List.foldr setClaim (Map.empty) cs

-- claimsList :: ClaimsMap -> [Claim]
-- claimsList cm = List.map claimFromMap $ Map.toList cm

-- claimFromMap :: ((Row, Col), PlayerId) -> Claim
-- claimFromMap ((r, c), p) = Claim (Tile r c) (Just p)

-- setClaim :: Claim -> ClaimsMap -> ClaimsMap
-- -- setClaim (Claim _ Nothing) b = b
-- setClaim (Claim (Tile row col) own) b = Map.insert (row, col) own b

-- gridMap :: (Int -> Int -> a -> b) -> Vector (Vector a) -> Vector (Vector b)
-- gridMap f rows =
  -- imap rows $ \r row ->
    -- imap row $ \c v ->
      -- f r c v
  -- where imap = flip Vector.imap

-- -- and then it's an easy List.concat to flatten it
-- gridToList :: Vector (Vector a) -> [[a]]
-- gridToList = Vector.toList . Vector.map Vector.toList

-- -- that's access, but now how do I WRITE a value
-- -- vector is hte WRONG
-- -- ok, no, DEFINITELY use HashMap
-- (!!?) :: Vector (Vector a) -> (Int, Int) -> Maybe a
-- grid !!? (r, c) = do
    -- row <- grid !? r
    -- val <- row !? c
    -- return val

---------------------------------------------------------------

setClaim :: Claim -> Board -> Board
setClaim (Claim _ Nothing) b = b
setClaim (Claim t (Just p)) b = setGrid (Map.insert t p (grid b)) b

movesLeft :: [Tile] -> Board -> Bool
movesLeft = undefined

-- I need to map it to a multi-deminsional list
showBoard :: Board -> String
showBoard b = unlines $ List.map line $ List.map (List.concat . List.map showClaim) $ List.chunksOf (Board.cols b) $ allClaims b
  where
    line cs = "|" ++ cs ++ "|"


showClaim :: Claim -> String
showClaim (Claim t (Just (p:ps))) = [p]
showClaim (Claim t (Just "")) = '?':""
showClaim (Claim _ Nothing) = ' ':""

-- \033[31m This text is red \033[0m

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard


























-- takenClaims :: ClaimsMap -> [Claim]
-- takenClaims b = map toClaim $ HM.toList b
  -- where
    -- toClaim ((r, c), p) = Claim (Tile r c) (Just p)

-- gameBoard :: Game -> Board
-- gameBoard game = board (Game.rows game) (Game.cols game) (Game.claims game)

-- board :: Rows -> Cols -> [Claim] -> Board
-- board rows cols cs = Board rows cols $ buildClaims cs

-- claim :: Board -> Tile -> Maybe Claim
-- claim board (Tile r c) =
    -- let mp = HM.lookup (r, c) (claimsMap board)
        -- tile = Tile r c
    -- in
    -- if validTile board tile
      -- then Just $ Claim tile mp
      -- else Nothing

-- hasOwner :: Claim -> Bool
-- hasOwner claim = isJust (owner claim)

-- bestMove :: PlayerId -> [Tile] -> Board -> Maybe (Int, Tile)
-- bestMove _ [] _ = Nothing
-- bestMove p ts b =
    -- let boards = map (placeTile p b) ts
        -- scores = map (playerScore p . takenClaims . claimsMap) boards
        -- both = zip scores ts
        -- best = maximumBy (compare `on` fst) both
    -- in Just best

-- placeTile :: PlayerId -> Board -> Tile -> Board
-- placeTile p b t =
    -- let army = neighborArmy p t b
        -- winner = winningPlayer p army
    -- in foldr (conquerClaim winner) b army

-- winningPlayer :: PlayerId -> [Claim] -> PlayerId
-- winningPlayer pid [] = pid
-- winningPlayer pid cs =
    -- let owners = nub $ catMaybes $ map owner cs
        -- scores = map (\p -> playerScore p cs) owners
        -- both = zip owners scores :: [(PlayerId, Int)]
        -- max = maximumBy (compare `on` snd) both
        -- pScore = fromMaybe 0 $ lookup pid both
    -- in win (pid, pScore) max
  -- where
    -- win (p, ps) (o, os)
      -- | ps > os  = p
      -- | ps == os = p
      -- | ps < os  = o

-- -- returns the elements!
-- playerScore :: PlayerId -> [Claim] -> Int
-- playerScore p cs = length $ filter (== p) $ catMaybes $ map owner cs

-- conquerClaim :: PlayerId -> Claim -> Board -> Board
-- conquerClaim p c b =
    -- let cmap = setClaim (c { owner = Just p }) (claimsMap b)
    -- in b { claimsMap = cmap }

-- neighbors :: Tile -> [Tile]
-- neighbors (Tile r c) =
    -- [                   Tile (r+1) (c)
    -- , Tile (r)   (c-1),                 Tile (r)   (c+1)
    -- ,                   Tile (r-1) (c)
    -- ]

-- validTile :: Board -> Tile -> Bool
-- validTile (Board rows cols _) (Tile r c) = r >= 0 && c >= 0 && r < rows && c < cols

-- neighborArmy :: PlayerId -> Tile -> Board -> [Claim]
-- neighborArmy playerId tile board =
  -- let ts = neighbors tile
      -- new = Claim tile (Just playerId)
  -- in new : List.filter hasOwner (mapMaybe (claim board) ts)

-- ----------------------------------------------------------

sampleBoard :: Board
sampleBoard = buildBoard 2 2 sampleClaims

sampleClaims :: [Claim]
sampleClaims = [Claim (Tile 0 1) (Just "bob"), Claim (Tile 1 0) (Just "alice")]

