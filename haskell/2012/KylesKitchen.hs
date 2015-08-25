module Main where

import Control.Monad
import System.IO

import Data.Maybe (catMaybes)
import Data.List (nub, sortBy, subsequences, maximumBy)
import Data.Function (on)
import qualified Data.Vector as V

-- MATRIX IMPORTS ----------------------------------------------------
import Data.Vector (Vector)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.List as L
import qualified Data.Vector as V
---------------------------------------------------------------------

import Woot (woot)

type X = Int
type Y = Int
type W = Int
type H = Int
data Pos = Pos X Y
         deriving (Show, Eq)

type Tile = Matrix Int

type Grid = Matrix Int

data Placement = Placement Int Grid
               deriving (Show, Eq)

tileA = matrixFromList [[1,1,1,1]]
tileB = matrixFromList [[1,0],[1,0],[1,1]]
tileC = matrixFromList [[1,1],[1,1]]
tileD = matrixFromList [[1,0],[1,1],[0,1]]
tileE = matrixFromList [[1,0],[1,1],[1,0]]

tileFromType :: String -> Maybe Tile
tileFromType "A" = Just tileA
tileFromType "B" = Just tileB
tileFromType "C" = Just tileC
tileFromType "D" = Just tileD
tileFromType "E" = Just tileE
tileFromType _ = Nothing

grid :: W -> H -> Grid
grid w h = matrixFromSize w h 0

-- map through the grid, and see if 
placeTileAt :: Tile -> Grid -> Pos -> Maybe Grid
placeTileAt t g p =
    if not (isOnGrid t p g) then Nothing else
    Just $ matrixIMap (updateCell t p) g

allPositions :: W -> H -> [Pos]
allPositions w h = [Pos x y | x <- [0..w-1], y <- [0..h-1]]

updateCell :: Tile -> Pos -> X -> Y -> Int -> Int
updateCell t (Pos x y) cx cy val = val + tileValue t (Pos (cx-x) (cy-y))

tileValue :: Tile -> Pos -> Int
tileValue t (Pos x y) = fromMaybe 0 (t !? (x, y))

-- can only be off the bottom and right
isOnGrid :: Tile -> Pos -> Grid -> Bool
isOnGrid t (Pos x y) g =
    let (tw, th) = dimensions t
        (gw, gh) = dimensions g
    in x + tw <= gw && y + th <= gh

-- now I also need to include all variants of the tile
placements :: Grid -> Tile -> [Grid]
placements g t =
    let (w, h) = dimensions g
    in filter isValidPlacement $ catMaybes $ map (placeTileAt t g) $ allPositions w h

placementsAllPerms :: Grid -> Tile -> [Grid]
placementsAllPerms g t = concat $ map (placements g) $ tilePermutations t

isValidPlacement :: Grid -> Bool
isValidPlacement (Matrix m) = V.all (V.all (<2)) m

-- tile, all rotations
-- flip, all rotations
tilePermutations :: Tile -> [Tile]
tilePermutations t = nub $ tileAllRotations t <> tileAllRotations (matrixFlipHorizontal t) <> tileAllRotations (matrixFlipVertical t)

tileAllRotations :: Tile -> [Tile]
tileAllRotations t = (take 4 $ iterate matrixRotate t)

maxTiles :: Grid -> [Tile] -> Int
maxTiles g ts =
    let tss = subsequences ts
        gs = map (\ts -> placeAllTiles ts g) tss
        scores = map (uncurry scoreGrid) $ zip tss gs
    in maximum scores

scoreGrid :: [Tile] -> [Grid] -> Int
scoreGrid _ [] = 0
scoreGrid ts _ = length ts

placeAllTiles :: [Tile] -> Grid -> [Grid]
placeAllTiles []     g = [g]
placeAllTiles (t:ts) g =
    let gs = placementsAllPerms g t
    in concatMap (placeAllTiles ts) gs

run :: Handle -> IO ()
run h = do
    mnLine <- hGetLine h
    tilesLine <- hGetLine h
    let [n, m] = map read $ words mnLine :: [Int]
        tiles = catMaybes $ map tileFromType $ words tilesLine
        g = grid n m
        max = maxTiles g tiles
    print max

test :: FilePath -> IO ()
test p = openFile p ReadMode >>= run


main = run stdin
































-- MATRIX FUNCTIONS --------------------------------------------------
newtype Matrix a = Matrix (Vector (Vector a))
                 deriving (Eq)

instance (MShow a) => Show (Matrix a) where
    show m = "\n" <> (L.intercalate "\n" $ map showRow (matrixToList m))
      where
        showRow :: MShow a => [a] -> String
        showRow r =(concat (map mshow r))

class MShow a where
    mshow :: a -> String

instance MShow Char where
    mshow c = [c]

instance MShow Int where
    mshow = show

instance MShow Bool where
    mshow True  = "X"
    mshow False = "O"

(!) :: Matrix a -> (Int, Int) -> a
(Matrix m) ! (x, y) = m V.! y V.! x

(!?) :: Matrix a -> (Int, Int) -> Maybe a
(Matrix m) !? (x, y) = do
  row <- (m V.!? y)
  val <- row V.!? x
  return val

matrixIMap :: (Int -> Int -> a -> b) -> Matrix a -> Matrix b
matrixIMap f (Matrix m) = Matrix $ V.imap eachRow m
  where
    eachRow y row   = V.imap (eachCol y) row
    eachCol y x val = f x y val

dimensions :: Matrix a -> (Int, Int)
dimensions (Matrix m) =
    let h = V.length m
        w = fromMaybe 0 $ V.length <$> (m V.!? 0)
    in (w, h)

-- rotates clockwise
matrixRotate :: Matrix a -> Matrix a
matrixRotate m =
  let (w', h') = dimensions m
      w = h'
      h = w'
  in Matrix $ V.generate h $ \y ->
     V.generate w $ \x ->
     m ! (y, (w - x - 1))

-- flips horizontal
matrixFlipHorizontal :: Matrix a -> Matrix a
matrixFlipHorizontal (Matrix m) = Matrix $ V.map V.reverse m

matrixFlipVertical :: Matrix a -> Matrix a
matrixFlipVertical (Matrix m) = Matrix (V.reverse m)

-- WARNING! Make sure to pad if there's a chance that the rows aren't even
-- use matrixPad
matrixFromList :: [[a]] -> Matrix a
matrixFromList rows = Matrix $ V.fromList (map V.fromList rows)

matrixPad :: a -> Matrix a -> Matrix a
matrixPad pad (Matrix m) = Matrix $ V.map (padRow (vectorsMaxWidth m) pad) m

vectorsMaxWidth :: Vector (Vector a) -> Int
vectorsMaxWidth rows = V.maximum $ V.map V.length rows

padRow :: Int -> a -> Vector a -> Vector a
padRow w p row =
    let n = w - V.length row
    in row <> V.replicate n p

matrixFromSize :: Int -> Int -> a -> Matrix a
matrixFromSize w h def = Matrix $ V.replicate h (V.replicate w def)

matrixToList :: Matrix a -> [[a]]
matrixToList (Matrix m) = map V.toList $ V.toList m
---------------------------------------------------------------------




