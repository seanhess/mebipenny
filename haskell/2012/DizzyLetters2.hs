module Main where

-- MATRIX IMPORTS ----------------------------------------------------
import Data.Vector (Vector)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.List as L
import qualified Data.Vector as V
---------------------------------------------------------------------

type Width = Int
type Height = Int
type X = Int
type Y = Int

type Art = Matrix Char

data Rotation = R0 | R90 | R180 | R270
              deriving (Show, Eq)

newtype Rotations = Rotations Int
                  deriving (Show, Eq)

rotations :: Int -> Rotations
rotations deg = Rotations $ pos $ (deg `div` 90) `rem` 4
  where
    pos n
      | n < 0 = n + 4
      | otherwise = n

rotate :: Art -> Art
rotate = matrixRotate

rotateN :: Rotations -> Art -> Art
rotateN (Rotations n) art = iterate rotate art !! n

bounds :: Width -> Height -> ((X,Y), (X,Y))
bounds w h = ((0,0),(w-1,h-1))

charsHeight :: [[Char]] -> Int
charsHeight = length

fromList :: [[Char]] -> Art
fromList css = matrixPad ' ' $ matrixFromList css

toList :: Art -> [[Char]]
toList = matrixToList

printChars :: [[Char]] -> IO ()
printChars css = mapM_ putStrLn css

example = [ "o...o"
          , ".o.o."
          , "..o.."
          , "..o.."
          , "..o.."
          , "..o.."
          ]

example' = [ ".....o"
           , "....o."
           , "oooo.."
           , "....o."
           , ".....o"
           ]

example2 = [ "123", "456", "789" ]
example3 = [ "123", "ABC" ]
example4 = [ "123" ]
example5 = [ "12", "AB" ]

--------------------------------------------------
main = do
    deg <- read <$> getLine
    ls <- lines <$> getContents
    let art = fromList ls
        art' = rotateN (rotations deg) art
        out = toList art'
    printChars out



-- MATRIX FUNCTIONS --------------------------------------------------
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

