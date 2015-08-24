module Main where

import Control.Monad
import System.IO
import Data.Vector ()

import Woot (woot)

test :: FilePath -> IO ()
test p = openFile p ReadMode >>= run

type X = Int
type Y = Int
type W = Int
type H = Int
data Pos = Pos X Y
         deriving (Show, Eq)

data Tile = Tile [Pos]
          deriving (Show, Eq)

-- filled positions
data Grid = Grid W H [Pos]
          deriving (Show, Eq)

-- Nope, I want to use a matrix instead!
-- whee!
-- then I can add them
-- and see if anything is 2!
-- wahoo!

-- maybe I should write a generalized rotate and flip function for multi-dimensional arrays
-- oh, yes, I definitely should
-- I wonder if it would be easier use a multi-dimensional vector than an array
-- yeah, let's try it

tileA = Tile [Pos 0 0, Pos 1 0, Pos 2 0, Pos 3 0]
tileB = undefined
tileC = undefined
tileD = undefined
tileE = undefined

rotate :: [Pos] -> [Pos]
rotate = undefined

flip :: [Pos] -> [Pos]
flip = undefined

tileFromType :: Char -> Maybe Tile
tileFromType 'A' = Just tileA
tileFromType 'B' = Just tileB
tileFromType 'C' = Just tileC
tileFromType 'D' = Just tileD
tileFromType 'E' = Just tileE
tileFromType _ = Nothing

-- it should return the grid too, right?
-- no it's easy enough to do that elsewhere
allPlacements :: Grid -> Tile -> [Grid]
allPlacements = undefined

-- where am I placing it though?
placeTile :: Grid -> Tile -> Pos -> Grid
placeTile = undefined

isValidPlacement :: Grid -> Tile -> Pos -> Bool
isValidPlacement = undefined

-- output is the maximum number of tiles Kyle can fit on his floor
-- so run the permutations and find the max?
-- we can flip and rotate them
-- I can also pre-flip and pre-rotation them?
-- sure, kind of

run :: Handle -> IO ()
run h = do
    print "HI"
    --codes <- lines <$> hGetContents h
    ---- lines cleans it up a lot
    --forM_ codes $ \code -> do
      --putStrLn "HI"

main = run stdin


