import System.IO

import Data.Char
import Control.Applicative
import Control.Monad

-- GRID IMPORTS ----------------------------------------------------
import Prelude hiding (lookup)
import Data.Vector (Vector, (//))
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid ((<>))
import qualified Data.List as L
import qualified Data.Vector as V
---------------------------------------------------------------------

gridContains :: Eq a => Grid a -> Grid a -> Bool
gridContains big small = any (gridContainsAt big small) $ allLocations big

gridContainsAt :: Eq a => Grid a -> Grid a -> (Int, Int) -> Bool
gridContainsAt big small (r, c) =
    all (gridMatchesAt big small (r, c)) $ allLocations small

gridMatchesAt :: Eq a => Grid a -> Grid a -> (Int, Int) -> (Int, Int) -> Bool
gridMatchesAt big small (r, c) (or, oc) =
    let bigLoc = (r+or, c+oc)
        smallLoc = (or, oc)
    in big !? bigLoc == small !? smallLoc

-- I could have done this with imap too!
allLocations :: Grid a -> [(Int, Int)]
allLocations g = do
    let (rows, cols) = dimensions g
    r <- [0..rows-1]
    c <- [0..cols-1]
    return (r, c)


big = fromList ' ' ["123","456","789"]
small = fromList ' ' ["23","56"]
small' = fromList ' ' ["23","89"]

testFile :: FilePath -> IO ()
testFile p = openFile p ReadMode >>= run

test = testFile "test.txt"

getGrid :: Handle -> IO (Grid Int)
getGrid h = do
    mnl <- hGetLine h
    let [r, c] = map read $ words mnl
    mlines <- replicateM r (hGetLine h)
    let nss = map (map digitToInt) mlines :: [[Int]]
    return $ fromList 0 nss

boolOut False = "NO"
boolOut True = "YES"

run :: Handle -> IO ()
run h = do
    t <- read <$> hGetLine h
    inputs <- replicateM t $ do
      big <- getGrid h
      small <- getGrid h
      return (big, small)
    let outs = map (\(b,s) -> boolOut $ gridContains b s) inputs
    mapM_ putStrLn outs

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
---------------------------------------------------------------------

