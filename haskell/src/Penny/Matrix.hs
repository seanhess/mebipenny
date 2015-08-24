
module Penny.Matrix where

-- PASTE IMPORTS ----------------------------------------------------
import Data.Vector (Vector)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.List as L
import qualified Data.Vector as V
---------------------------------------------------------------------


-- PASTE FUNCTIONS --------------------------------------------------
newtype Matrix a = Matrix (Vector (Vector a))
                 deriving (Eq)

instance (MShow a) => Show (Matrix a) where
    show m = L.intercalate "\n" $ map showRow (matrixToList m)
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

matrixToList :: Matrix a -> [[a]]
matrixToList (Matrix m) = map V.toList $ V.toList m
---------------------------------------------------------------------

