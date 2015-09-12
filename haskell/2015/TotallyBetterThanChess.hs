module Main where

import Prelude hiding (lookup)
import System.IO
import Data.Char
import Data.List as List hiding (lookup)
import qualified Data.List.Split as List
import Debug.Trace
import Control.Monad
import Data.Maybe
import Data.Monoid ((<>), mconcat)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Vector as Vector
import Data.Vector (Vector, (//))
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Vector as V
import Debug.Trace



-- space with stone = 0
-- continguous if related horizontally or vertically
-- each cell inside a contiguous region of spaces surrounded by a single color is worth one point for that color.
  -- also counts if against a wall
-- each cell touched by both black and white is worth nothing
-- boards are always square

data Piece = B | W | E deriving (Show, Eq)

piece :: String -> Piece
piece "B" = B
piece "W" = W
piece "_" = E

isSpace :: Piece -> Bool
isSpace E = True
isSpace _ = False

-- whosePoint :: Grid Piece -> Row -> Col -> Piece -> Piece
-- whosePoint g r c p
  -- | allWhite = W
  -- | allBlack = B
  -- | otherwise = E

  -- where
    -- surr =
    -- allWhite = all (== W) surr
    -- allBlack = all (== B) surr

scoreAll :: Grid Piece -> [(Piece, Int)]
scoreAll g = mapMaybe (scoreRegion g) $ regions g

scoreRegion :: Grid Piece -> [Location] -> Maybe (Piece, Int)
scoreRegion g ls
  | not (allSpaces ps) = Nothing
  | allBlack surr = Just (B, length ls)
  | allWhite surr = Just (W, length ls)
  | otherwise = Nothing
  where
    ps = map (g !) ls
    surr = allSurrounding g ls

allSpaces :: [Piece] -> Bool
allSpaces = all (== E)
allBlack :: [Piece] -> Bool
allBlack = all (\p -> p == B || p == E)
allWhite :: [Piece] -> Bool
allWhite = all (\p -> p == W || p == E)

-- allSpaces :: [Piece]
-- allSpaces = all $ 

allSurrounding :: Grid Piece -> [Location] -> [Piece]
allSurrounding g ls = concatMap (surrounding g) ls

surrounding :: Grid Piece -> Location -> [Piece]
surrounding g (r, c) = map (g !) $ adjacent g (r, c)

findRegion :: Grid Piece -> [Location] -> Location -> [Location]
findRegion g visited l =
    let me = g ! l :: Piece
        as = adjacent g l :: [Location]
        unvisited = filter (not . (`elem` visited)) as :: [Location]
        neighbors = map (g !) unvisited
        both = zip unvisited neighbors :: [(Location, Piece)]
        good = filter (same me) both
    in
    l : concatMap (findRegion g (l:visited) . fst) good

-- each pass, update visited, and find the region
regions :: Grid Piece -> [[Location]]
regions g = snd $ foldr (regionAt g) ([], []) $ allLocations g

isVisited :: [Location] -> Location -> Bool
isVisited visited l = l `elem` visited

-- loop through... what?


regionAt :: Grid Piece -> Location -> ([Location], [[Location]]) -> ([Location], [[Location]])
regionAt g l (visited, all)
  | isVisited visited l = (visited, all)
  | otherwise =
      let region = findRegion g visited l
      in (region ++ visited, region : all)

    -- if l is in visited, return nothing

same :: Piece -> (Location, Piece) -> Bool
same me (l, v) = v == me

sample :: Grid Piece
sample = fromList E $ map (map piece . words)
  [ "B _ _ _"
  , "B B B B"
  , "W W W W"
  , "W _ _ W"
  ]


-- tally :: [(Piece, Int)] -> [

isBlack :: (Piece, Int) -> Bool
isBlack (B,_) = True
isBlack _ = False

isWhite :: (Piece, Int) -> Bool
isWhite (W,_) = True
isWhite _ = False

-- I also have to be surrounded by the color...
-- to just all of them  be the same
-- the whole region of spaces must be surrounded, not just this one...

-- do this for a whole region of spaces

testFile :: FilePath -> IO ()
testFile p = openFile p ReadMode >>= run

test = testFile "test.txt"

-- sample :: Grid Piece
-- sample = fromList E $ map (map piece . words)
  -- [ "B _ _ _"
  -- , "B B B B"
  -- , "W W W W"
  -- , "W _ _ W"
  -- ]

run :: Handle -> IO ()
run h = do
    nss <- getLines h :: IO [String]
    let g = fromList E $ map (map piece . words) nss :: Grid Piece
        scores = scoreAll g
        b = filter isBlack scores
        w = filter isWhite scores
    putStrLn $ "Black: " ++ show (sum $ map snd b)
    putStrLn $ "White: " ++ show (sum $ map snd w)
    -- let outs = map (showResult . result) nss
    -- mapM_ putStrLn outs

    return ()


---------------------------------------------------------
-- reading

getLines :: Handle -> IO [String]
getLines h = lines <$> hGetContents h

getNLines :: Handle -> Int -> IO [String]
getNLines h n = replicateM n (hGetLine h)

-- plus hGetLine h!

----------------------------------------------------------
-- parsing

parseReads :: Read a => String -> [a]
parseReads = map read . words

parseInts :: String -> [Int]
parseInts = parseReads

parseInt :: String -> Int
parseInt = read

parseWords :: String -> [String]
parseWords = words

main = run stdin


































































-- GRID FUNCTIONS --------------------------------------------------
type Col = Int
type Row = Int
type Rows = Int
type Cols = Int
type Grid a = Vector (Vector a)
type Location = (Int, Int)

showGrid :: GShow a => Grid a -> String
showGrid m = "\n" <> (L.intercalate "\n" $ map showRow (toList m))
  where
    showRow :: GShow a => [a] -> String
    showRow r =(concat (map gshow r))

class GShow a where
    gshow :: a -> String

instance GShow Char where
    gshow c = [c]

instance GShow Int where
    gshow = show

instance GShow Bool where
    gshow True  = "X"
    gshow False = "O"

(!) :: Grid a -> Location -> a
grid ! loc =
  case lookup loc grid of
    Nothing -> error (show loc ++ " out of bounds")
    Just v -> v

(!?) :: Grid a -> Location -> Maybe a
grid !? loc = lookup loc grid

lookup :: Location -> Grid a -> Maybe a
lookup (r, c) grid = do
  row <- grid V.!? r
  val <- row V.!? c
  return val


isValid :: Grid a -> Location -> Bool
isValid grid (r, c) =
    let (rows, cols) = dimensions grid
    in isValidBounds rows r && isValidBounds cols c

isValidBounds :: Int -> Int -> Bool
isValidBounds len i = i >= 0 && i < len

imap :: (Int -> Int -> a -> b) -> Grid a -> Grid b
imap f grid = V.imap eachRow grid
  where
    eachRow r row   = V.imap (eachCol r) row
    eachCol r c val = f r c val

dimensions :: Grid a -> (Rows, Cols)
dimensions grid = (lengthRows grid, lengthCols grid)

lengthRows :: Grid a -> Rows
lengthRows = V.length

lengthCols :: Grid a -> Cols
lengthCols grid = fromMaybe 0 $ V.length <$> (grid V.!? 0)

rotateClockwise :: Grid a -> Grid a
rotateClockwise m =
  let (oldRows, oldCols) = dimensions m
      rows = oldCols
      cols = oldRows
  in V.generate rows $ \r ->
      V.generate cols $ \c ->
        m ! ((cols - c - 1), r)

--------------------------------------------------------------
update :: Grid a -> [(Location, a)] -> Grid a
update g pairs =
  -- you can't map, you don't which is which
  let rowUps = L.map (updateRow g) $ L.groupBy sameRow pairs
  in g // rowUps

updateRow g pairs =
  let r = (groupRow pairs)
      row = g V.! r
      cps = map toColumnPair pairs
      row' = row // cps
  in (r, row')

toColumnPair p = (pairCol p, pairVal p)

groupRow (p:ps) = pairRow p
sameRow p1 p2 = pairRow p1 == pairRow p2
pairRow ((r, _), _) = r
pairCol ((_, c), _) = c
pairVal (_, v) = v
-------------------------------------------------------

-- flips horizontal
flipHorizontal :: Grid a -> Grid a
flipHorizontal grid = V.map V.reverse grid

flipVertical :: Grid a -> Grid a
flipVertical grid = V.reverse grid

fromList :: a -> [[a]] -> Grid a
fromList def rows = pad def $ V.fromList (map V.fromList rows)

fromIntList = fromList 0
fromStrList = fromList ' '

empty :: Int -> Int -> a -> Grid a
empty w h def = V.replicate h (V.replicate w def)

pad :: a -> Grid a -> Grid a
pad p grid = V.map (padRow (maxWidth grid) p) grid

maxWidth :: Vector (Vector a) -> Int
maxWidth rows = V.maximum $ V.map V.length rows

padRow :: Int -> a -> Vector a -> Vector a
padRow w p row =
    let n = w - V.length row
    in row <> V.replicate n p

toList :: Grid a -> [[a]]
toList grid = map V.toList $ V.toList grid

adjacent :: Grid a -> Location -> [Location]
adjacent g (r, c) =
    L.filter (isValid g)
    [          (r+1, c)
    , (r, c-1),          (r, c+1)
    ,          (r-1, c)
    ]

allLocations :: Grid a -> [(Int, Int)]
allLocations g = do
    let (rows, cols) = dimensions g
    r <- [0..rows-1]
    c <- [0..cols-1]
    return (r, c)
---------------------------------------------------------------------

