{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Board where


import Data.Hashable
import Data.HashMap.Strict as Map
import Data.Maybe as Maybe
import Data.HashMap.Strict (HashMap)
import Data.List as List
import Data.Function (on)
import Data.List.Split as List

type Col = Int
type Row = Int
type Rows = Int
type Cols = Int

class (Hashable a, Eq a, Show a) => Loc a where
  row :: a -> Int
  col :: a -> Int
  location :: Int -> Int -> a

class ShowChar a where
  showc :: a -> Char

instance Loc (Int, Int) where
  row = fst
  col = snd
  location = (,)

-- no, this is all the pieces in play. It's the most natural
data Board loc piece = Board
  { rows :: Rows
  , cols :: Cols
  , grid :: (Loc loc) => HashMap loc piece
  }

setGrid :: (Loc loc) => HashMap loc piece -> Board loc piece -> Board loc piece
setGrid g b = b { grid = g }

-- what do I do to build a board?
-- I need assocs!
fromList :: Loc loc => Rows -> Cols -> [(loc, piece)] -> Board loc piece
fromList rows cols lps = List.foldr set (emptyBoard rows cols) lps
  where
    set (l, p) b = setPiece l p b

emptyBoard :: Rows -> Cols -> Board index piece
emptyBoard rows cols = Board rows cols Map.empty

-- what should this do if it's an invalid location?
-- return Nothing? Yes, but that means we screwed up
lookup :: Loc loc => loc -> Board loc piece -> Maybe piece
lookup l b = Map.lookup l (grid b)

lookupAll :: Loc loc => [loc] -> Board loc piece -> [(loc, Maybe piece)]
lookupAll ls b = List.zip ls $ List.map (flip Board.lookup b) ls
    -- ps = 
    -- in List.zip ls ps

allLocations :: Loc loc => Board loc p -> [loc]
allLocations (Board rows cols _) = do
    r <- [0..rows-1]
    c <- [0..cols-1]
    return $ location r c

emptyLocations  :: Loc loc => Board loc p -> [loc]
emptyLocations b = Maybe.mapMaybe each $ toListAll b
  where
    each (loc, Nothing) = Nothing
    each (loc, (Just p)) = Just loc

toListAll :: Loc loc => Board loc p -> [(loc, Maybe p)]
toListAll b = flip lookupAll b $ allLocations b

toListFilled :: Loc loc => Board loc p -> [(loc, p)]
toListFilled b = Maybe.mapMaybe each $ toListAll b
  where
    each (loc, Nothing) = Nothing
    each (loc, (Just p)) = Just (loc, p)


-- toList :: Loc loc => Board loc piece -> [(loc, piece)]
-- toList (Board _ _ grid) = Map.toList grid

-- emptyLocs :: Loc loc => Board loc p -> [loc]

-- TODO make sure it is a valid location first
setPiece :: Loc loc => loc -> piece -> Board loc piece -> Board loc piece
setPiece l p b = setGrid (Map.insert l p (grid b)) b

isValid :: Loc loc => Board loc piece -> loc -> Bool
isValid (Board rows cols _) l = row l >= 0 && row l < rows && col l >= 0 && col l < cols


-- I need to map it to a multi-deminsional list
showBoard :: (ShowChar piece, Loc loc) => Board loc piece -> String
showBoard b =
  let all = Board.toListAll b
      rows = List.chunksOf (cols b) all
      lns = List.map (List.map (showPiece . snd)) rows
  in unlines (List.map line lns)

  where
    line cs = "|" ++ cs ++ "|"
    showPiece Nothing  = ' '
    showPiece (Just p) = showc p

-- printBoard :: (ShowChar piece, Loc loc) => Board loc piece -> IO ()
-- printBoard = putStrLn . showBoard

instance (ShowChar piece, Loc loc) => Show (Board loc piece) where
    show = showBoard


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

sampleBoard :: Board (Int, Int) String
sampleBoard = Board.fromList 2 2 [((0,1),"bob"),((1,0),"alice")]

instance ShowChar String where
    showc [] = ' '
    showc (x:xs) = x

-- sampleClaims :: [Claim]
-- sampleClaims = [Claim (Tile 0 1) (Just "bob"), Claim (Tile 1 0) (Just "alice")]

