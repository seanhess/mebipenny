-- 1 hour for 2 points

import Debug.Trace
import System.IO

import Data.Maybe (catMaybes)
import Control.Monad

-- GRID IMPORTS ----------------------------------------------------
import Prelude hiding (lookup)
import Data.Vector (Vector, (//))
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid ((<>))
import qualified Data.List as L
import qualified Data.Vector as V
---------------------------------------------------------------------

data RGB = RGB Int Int Int
type Image = Grid RGB

instance Show RGB where
    show (RGB r g b) = unwords [show r, show g, show b]

sample :: Image
sample = fromList (RGB 0 0 0)
  [ [ RGB 75 35 5, RGB 15 255 35 ]
  , [ RGB 12 46 35, RGB 16 48 5 ]
  ]

pixelsForReplace :: Grid RGB -> [(Int, Int)]
pixelsForReplace grid =
  catMaybes $ concat $ toList $ imap each grid
  where
    each r c rgb =
      if needsReplace rgb
        then Just (r, c)
        else Nothing

needsReplace :: RGB -> Bool
needsReplace (RGB r g b) = g > (r + b)

getPixels :: [(Int, Int)] -> Grid RGB -> [((Int,Int), RGB)]
getPixels locs g = zip locs $ map (g !) locs

chromakey :: Grid RGB -> Grid RGB -> Grid RGB
chromakey first second =
    let pxs = pixelsForReplace first
        ups = getPixels pxs second
    in update first ups

-- replace all pixels in an image that fallin a certain color range
-- with pixels from another image

-- Line One: W
-- Line Two: H
-- R G B
-- WXH lines for the first image
-- WXH lines for the second image

-- each pixel in the first image,
  -- where the green value is greater than the combined red and blue value
  -- replace with the corresponding pixel in the second image

rgb :: String -> RGB
rgb xs =
    let [r, g, b] = map read $ words xs
    in RGB r g b

emptyRGB :: RGB
emptyRGB = RGB 0 0 0

test :: FilePath -> IO ()
test p = openFile p ReadMode >>= run

showImage :: Image -> String
showImage g = L.intercalate "\n" $ map show $ concat $ toList g

getImage :: Int -> Int -> Handle -> IO Image
getImage width height h = do
  rgbss <- replicateM width $ do
    ps <- replicateM height (hGetLine h)
    return $ map rgb ps
  return $ fromList emptyRGB rgbss

run :: Handle -> IO ()
run h = do
    width <- read <$> hGetLine h
    height <- read <$> hGetLine h
    let len = width * height
    first <- getImage width height h
    second <- getImage width height h
    let ch = chromakey first second
    putStrLn $ showImage ch

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
grid ! loc = fromJust $ lookup loc grid

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
  let (w', h') = dimensions m
      w = h'
      h = w'
  in V.generate h $ \y ->
     V.generate w $ \x ->
     m ! (y, (w - x - 1))

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



