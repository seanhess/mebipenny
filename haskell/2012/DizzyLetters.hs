module Main where

import Control.Applicative ((<$>))
import Data.Array (Array, ixmap, (!), listArray, elems)
import Data.List.Split (chunksOf)
import Data.List (transpose)
import Data.Monoid ((<>))

-- Lessons learned:
-- read CAREFULLY
-- think about edge cases
-- when they mention something like padding rows, stub out a function for it early on in the process

type Width = Int
type Height = Int
type X = Int
type Y = Int
data Art = Art Width Height (Array (X, Y) Char)
         deriving (Show)

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

----------------------------------------------------------

rotate :: Art -> Art
rotate (Art w h chars) = Art h w $ ixmap (bounds h w) (findOldXY h w) chars

-- 1 2 3    A 1
-- A B C    B 2
--          C 3

findOldXY :: Width -> Height -> (X,Y) -> (X,Y)
findOldXY w h (x, y) = (y, w-1-x)
-- 3x2 0 0 = A = 0       (h-1-x)
-- 3x2 1 0 = 1 = (w-1-y) (h-1-x)

rotateN :: Rotations -> Art -> Art
rotateN (Rotations n) art = iterate rotate art !! n
--------------------------------------------------------

bounds :: Width -> Height -> ((X,Y), (X,Y))
bounds w h = ((0,0),(w-1,h-1))

charsHeight :: [[Char]] -> Int
charsHeight = length

fromList :: [[Char]] -> Art
fromList css =
    let w = biggestWidth css
        h = charsHeight css
        rows = map (padRow w) css
    in Art w h (fromListArr w h rows)

-- todo pad with spaces to make it rectangular
fromListArr :: Width -> Height -> [[Char]] -> Array (X,Y) Char
fromListArr w h rows = listArray (bounds w h) (concat $ transpose rows)

biggestWidth :: [[a]] -> Int
biggestWidth = maximum . map length

padRow :: Width -> [Char] -> [Char]
padRow w cs =
    let n = w - length cs
    in cs <> replicate n ' '

toList :: Art -> [[Char]]
toList (Art w h chars) = transpose $ chunksOf h $ elems chars

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


