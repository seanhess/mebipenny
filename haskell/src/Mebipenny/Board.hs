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

class (Eq a, Show a) => Piece a where
  unoccupied :: a
  showc :: a -> Char

instance Loc (Int, Int) where
  row = fst
  col = snd
  location = (,)

instance Piece Char where
  unoccupied = '_'
  showc = id

-- no, this is all the pieces in play. It's the most natural
data Board l p = Board
  { rows :: Rows
  , cols :: Cols
  , grid :: (Loc l, Piece p) => HashMap l p
  }

setGrid :: (Loc l, Piece p) => HashMap l p -> Board l p -> Board l p
setGrid g b = b { grid = g }

-----------------------------------------------------------------------
-- Building and writing

fromList :: (Loc l, Piece p) => Rows -> Cols -> [(l, p)] -> Board l p
fromList rows cols lps = setPieces lps (emptyBoard rows cols)

emptyBoard :: Rows -> Cols -> Board index piece
emptyBoard rows cols = Board rows cols Map.empty

setPiece :: (Loc l, Piece p) => l -> p -> Board l p -> Board l p
setPiece l p b =
    if isValid b l
      then setGrid (Map.insert l p (grid b)) b
      else b

setPieces :: (Loc l, Piece p) => [(l, p)] -> Board l p -> Board l p
setPieces lps b = List.foldr (\(l, p) b -> setPiece l p b) b lps

isValid :: Loc loc => Board loc piece -> loc -> Bool
isValid (Board rows cols _) l = row l >= 0 && row l < rows && col l >= 0 && col l < cols

------------------------------------------------------------------------
-- Lookup

-- what should this do if it's an invalid location?
-- return Nothing? Yes, but that means we screwed up
lookup :: (Loc l, Piece p) => l -> Board l p -> p
lookup l b =
    case Map.lookup l (grid b) of
      Nothing -> unoccupied
      Just p -> p

lookupAll :: (Loc l, Piece p) => [l] -> Board l p -> [(l, p)]
lookupAll ls b = List.zip ls $ List.map (flip lookup b) ls

allLocations :: Loc loc => Board loc p -> [loc]
allLocations (Board rows cols _) = do
    r <- [0..rows-1]
    c <- [0..cols-1]
    return $ location r c

toList :: (Loc l, Piece p) => Board l p -> [(l, p)]
toList b = flip lookupAll b $ allLocations b


------------------------------------------------------------------------
-- Printing!

showBoard :: (Piece p, Loc l) => Board l p -> String
showBoard b =
  let rmax = rows b
      all = toList b
      rs = List.chunksOf (cols b) all
      lns = List.map (List.concatMap (showEach . snd)) rs
      top = " _" ++ List.concatMap showNum [0..rmax-1] ++ "__"
  in unlines (top : List.zipWith line [0..] lns)

  where
    line n cs = show n ++ "|" ++ cs ++ " |"
    showEach v = ' ' : showc v : ""
    showNum n = '_' : show n

instance (Piece p, Loc l) => Show (Board l p) where
    show = showBoard

----------------------------------------------------------------------

adjacent :: (Loc loc) => loc -> [loc]
adjacent loc =
    let r = row loc
        c = col loc
    in
    [                     location (r+1) (c)
    , location (r) (c-1),                     location (r)   (c+1)
    ,                     location (r-1) (c)
    ]

corners :: (Loc loc) => loc -> [loc]
corners loc =
    let r = row loc
        c = col loc
    in
    [ location (r-1) (c-1),                     location (r-1) (c+1)

    , location (r+1) (c-1),                     location (r+1) (c+1)
    ]



-- NOTES
-- owner can be null, playerId in the response
-- but it can be unnocupied too!
-- Tiles: explicitly unoccupied, maybe?
-- Piece: Empty | Wall | Tile PlayerId
-- hmmmmm.... maybe I should enforce that
-- I need to handle those explicitly when I predict what their response can be!

-- PASS is a valid move



-- TODO make a UI so you can play against your bot?
-- that would be very helpful I bet, but slow...
-- maybe a command-line UI?
-- probably not worth it





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

-- returns the elements!
-- playerScore :: PlayerId -> [Claim] -> Int
-- playerScore p cs = length $ filter (== p) $ catMaybes $ map owner cs

-- neighborArmy :: PlayerId -> Tile -> Board -> [Claim]
-- neighborArmy playerId tile board =
  -- let ts = neighbors tile
      -- new = Claim tile (Just playerId)
  -- in new : List.filter hasOwner (mapMaybe (claim board) ts)

-- ----------------------------------------------------------

sampleBoard :: Board (Int, Int) Char
sampleBoard = fromList 2 2 [((0,1),'b'),((1,0),'a')]

-- instance ShowChar String where
    -- showc [] = ' '
    -- showc (x:xs) = x

-- sampleClaims :: [Claim]
-- sampleClaims = [Claim (Tile 0 1) (Just "bob"), Claim (Tile 1 0) (Just "alice")]

