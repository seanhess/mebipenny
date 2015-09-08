
module Penny.Grid where

-- GRID IMPORTS ----------------------------------------------------
import Data.Vector (Vector)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.List as L
import qualified Data.Vector as V
---------------------------------------------------------------------


-- GRID FUNCTIONS --------------------------------------------------
type Grid a = Vector (Vector a)

showGrid :: GShow a => Grid a -> String
showGrid m = "\n" <> (L.intercalate "\n" $ map showRow (gridToList m))
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

(!) :: Grid a -> (Int, Int) -> a
m ! (x, y) = m V.! y V.! x

(!?) :: Grid a -> (Int, Int) -> Maybe a
grid !? (x, y) = do
  row <- grid V.!? y
  val <- row V.!? x
  return val

gridIMap :: (Int -> Int -> a -> b) -> Grid a -> Grid b
gridIMap f grid = V.imap eachRow grid
  where
    eachRow y row   = V.imap (eachCol y) row
    eachCol y x val = f x y val

dimensions :: Grid a -> (Int, Int)
dimensions grid =
    let h = V.length grid
        w = fromMaybe 0 $ V.length <$> (grid V.!? 0)
    in (w, h)

-- rotates clockwise
gridRotate :: Grid a -> Grid a
gridRotate m =
  let (w', h') = dimensions m
      w = h'
      h = w'
  in V.generate h $ \y ->
     V.generate w $ \x ->
     m ! (y, (w - x - 1))

-- flips horizontal
gridFlipHorizontal :: Grid a -> Grid a
gridFlipHorizontal grid = V.map V.reverse grid

gridFlipVertical :: Grid a -> Grid a
gridFlipVertical grid = V.reverse grid

-- WARNING! Make sure to pad if there's a chance that the rows aren't even
-- use gridPad
gridFromList :: [[a]] -> Grid a
gridFromList rows = V.fromList (map V.fromList rows)

gridFromSize :: Int -> Int -> a -> Grid a
gridFromSize w h def = V.replicate h (V.replicate w def)

gridPad :: a -> Grid a -> Grid a
gridPad pad grid = V.map (padRow (gridMaxWidth grid) pad) grid

gridMaxWidth :: Vector (Vector a) -> Int
gridMaxWidth rows = V.maximum $ V.map V.length rows

padRow :: Int -> a -> Vector a -> Vector a
padRow w p row =
    let n = w - V.length row
    in row <> V.replicate n p

gridToList :: Grid a -> [[a]]
gridToList grid = map V.toList $ V.toList grid
---------------------------------------------------------------------

