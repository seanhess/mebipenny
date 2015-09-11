{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Mebipenny.Board where

import Prelude hiding (lookup)
import Data.Hashable
import qualified Data.HashMap.Strict as Map
import qualified Data.Maybe as Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import Data.Function (on)
import qualified Data.List.Split as List

type Col = Int
type Row = Int
type Rows = Int
type Cols = Int

class (Hashable a, Eq a, Show a) => Loc a where
  row :: a -> Int
  col :: a -> Int
  location :: Int -> Int -> a

class (Eq a, Show a) => BoardPiece a where
  unoccupied :: a
  showc :: a -> Char

instance Loc (Int, Int) where
  row = fst
  col = snd
  location = (,)

instance BoardPiece Char where
  unoccupied = '_'
  showc = id

-- no, this is all the pieces in play. It's the most natural
data Board l p = Board
  { rows :: Rows
  , cols :: Cols
  , grid :: (Loc l, BoardPiece p) => HashMap l p
  }

instance (Loc l, BoardPiece p) => Eq (Board l p) where
  (Board r1 c1 g1) == (Board r2 c2 g2) =
    r1 == r2 &&
    c1 == c2 &&
    g1 == g2

setGrid :: (Loc l, BoardPiece p) => HashMap l p -> Board l p -> Board l p
setGrid g b = b { grid = g }

-----------------------------------------------------------------------
-- Building and writing

fromList :: (Loc l, BoardPiece p) => Rows -> Cols -> [(l, p)] -> Board l p
fromList rows cols lps = setPieces (emptyBoard rows cols) lps

emptyBoard :: (Loc l, BoardPiece p) => Rows -> Cols -> Board l p
emptyBoard rows cols = Board rows cols Map.empty

setPiece :: (Loc l, BoardPiece p) => Board l p -> l -> p -> Board l p
setPiece b l p =
    if isValid b l
      then setGrid (Map.insert l p (grid b)) b
      else b

setPieces :: (Loc l, BoardPiece p) => Board l p -> [(l, p)] -> Board l p
setPieces b lps = List.foldr (\(l, p) b -> setPiece b l p) b lps

isValid :: (Loc l, BoardPiece p) => Board l p -> l -> Bool
isValid (Board rows cols _) l = row l >= 0 && row l < rows && col l >= 0 && col l < cols

------------------------------------------------------------------------
-- Lookup

lookup :: (Loc l, BoardPiece p) => Board l p -> l -> p
lookup b l =
    case Map.lookup l (grid b) of
      Nothing -> unoccupied
      Just p -> p

lookupAll :: (Loc l, BoardPiece p) => Board l p -> [l] -> [(l, p)]
lookupAll b ls = List.zip ls $ List.map (lookup b) ls

allLocations :: (Loc l, BoardPiece p) => Board l p -> [l]
allLocations (Board rows cols _) = do
    r <- [0..rows-1]
    c <- [0..cols-1]
    return $ location r c

toList :: (Loc l, BoardPiece p) => Board l p -> [(l, p)]
toList b = lookupAll b $ allLocations b


------------------------------------------------------------------------
-- Printing!

showBoard :: (BoardPiece p, Loc l) => Board l p -> String
showBoard b =
  let rmax = rows b
      all = toList b
      rs = List.chunksOf (cols b) all
      lns = List.map (List.concatMap (showEach . snd)) rs
      top = " _" ++ List.concatMap showNum [0..rmax-1] ++ "__"
  in "\n" ++ unlines (top : List.zipWith line [0..] lns)

  where
    line n cs = show (n `mod` 10) ++ "|" ++ cs ++ " |"
    showEach v = ' ' : showc v : ""
    showNum n = '_' : show (n `mod` 10)

instance (BoardPiece p, Loc l) => Show (Board l p) where
    show = showBoard

----------------------------------------------------------------------

-- should this filter valid spaces?
-- what about walls, etc?
-- they'll come back as Empty
-- not sure if that makes sense
adjacent :: (Loc l, BoardPiece p) => Board l p -> l -> [l]
adjacent b loc =
    let r = row loc
        c = col loc
    in List.filter (isValid b)
    [                     location (r+1) (c)
    , location (r) (c-1),                     location (r)   (c+1)
    ,                     location (r-1) (c)
    ]

corners :: (Loc l, BoardPiece p) => Board l p -> l -> [l]
corners b loc =
    let r = row loc
        c = col loc
    in List.filter (isValid b)
    [ location (r-1) (c-1),                     location (r-1) (c+1)

    , location (r+1) (c-1),                     location (r+1) (c+1)
    ]

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

-- returns the elements!
-- playerScore :: PlayerId -> [Claim] -> Int
-- playerScore p cs = length $ filter (== p) $ catMaybes $ map owner cs

-- ----------------------------------------------------------

-- sampleBoard :: Board (Int, Int) Char
-- sampleBoard = fromList 2 2 [((0,1),'b'),((1,0),'a')]

-- instance ShowChar String where
    -- showc [] = ' '
    -- showc (x:xs) = x

-- sampleClaims :: [Claim]
-- sampleClaims = [Claim (Tile 0 1) (Just "bob"), Claim (Tile 1 0) (Just "alice")]

