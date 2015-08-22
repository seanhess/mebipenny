module Main where

import Control.Monad (replicateM, liftM2)
import Control.Applicative ((<$>))
import Data.Char (intToDigit)
import Data.HashMap.Strict (HashMap, fromList)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust, catMaybes)
import Data.List (find, sort, nub)
import Data.Monoid ((<>))
import Test.QuickCheck

-- how do you represent the adjacent state?
-- I'm going to go through and pre-calculate that?
-- I could, or I could not. 
data CellState = HasMine | Adjacent | Clear deriving (Show, Eq)

--handleClick Mine = lose
--handleClick (Adjacent n) = show, reveal the count
--handleClick (Clear) = show, and reveal all adjacent cells, if clear, continue

-- give a grid size, a description of mine locations, and a click location, provide all the of the cell locations that are unobscured.

-- ok, let's have a cell
-- I might need an Array for this instead of List

-- have a grid. a whole game state, just run the whole operation
-- I need a way to determine adjacent cells, primarily

-- you start with 1 cell1 cell, the clicked one
-- if they are Clear, then also reveal their neighbors. 
-- oh, return the cells that should be cleared by you:

type X = Int
type Y = Int
type Width = Int
type Height = Int

type Address = String
type Revealed = Bool

type Minefield = HashMap Address Bool

data Size = Size Width Height deriving (Show, Eq)

data Location = Location X Y deriving (Eq)

-- I just need to return things for each cell
cellState :: Size -> Minefield -> Location -> CellState
cellState size ms l
  | hasMine ms l = HasMine
  | isJust (find (hasMine ms) (neighbors size l)) = Adjacent
  | otherwise = Clear

hasMine :: Minefield -> Location -> Bool
hasMine ms l = HM.member (locationAddress l) ms

address :: X -> Y -> Address
address x y = concat [show x, "-", show y]

locationAddress :: Location -> Address
locationAddress (Location x y) = address x y

makeMinefield :: [Location] -> Minefield
makeMinefield ms = HM.fromList $ map placeMine ms
  where placeMine (Location x y) = (address x y, True)

neighbors :: Size -> Location -> [Location]
neighbors (Size w h) l = filter (isValid w h) $ allNeighbors l
  where
    allNeighbors (Location x y) =
      [ Location (x-1) (y-1), Location (x) (y-1), Location (x+1) (y-1)
      , Location (x-1) (y), Location (x+1) (y)
      , Location (x-1) (y+1), Location (x) (y+1), Location (x+1) (y+1)
      ]

isValid :: Width -> Height -> Location -> Bool
isValid w h (Location x y) = x >= 0 && y >= 0 && x < w && y < h

-- the issue is they are going to re-reveal their neighbors. like over and over


-------------------------------------------------------------------------------

-- revealed cells, only recurse if my state is 
revealedCells :: Size -> Minefield -> [Location] -> Location -> [Location]
revealedCells size ms visited l =
  case cellState size ms l of
    HasMine -> [l]
    Adjacent -> [l]
    Clear -> l : (nub $ concat (map nextReveals next))
  where

    next = nextNeighbors size visited l

    nextReveals :: Location -> [Location]
    nextReveals = revealedCells size ms ([l] ++ next ++ visited)

    -- then get their revealed cells and flatten

nextNeighbors :: Size -> [Location] -> Location -> [Location]
nextNeighbors size vs l = filter (notVisited vs) $ neighbors size l

notVisited :: [Location] -> Location -> Bool
notVisited visited = (`notElem` visited)

readLocation :: String -> Location
readLocation s = let [x, y] = map read (words s) in Location x y

readFirst :: String -> (Width, Height, Int)
readFirst s = let [w, h, n] = map read (words s) in (w, h, n)

instance Show Location where
    show (Location x y) = concat [show x, " ", show y]

instance Ord Location where
    compare (Location x1 y1) (Location x2 y2) = compare (x1, y1) (x2, y2)
    (Location x1 y1) <= (Location x2 y2) = (x1, y1) <= (x2, y2)



main = do
    (w, h, n) <- readFirst <$> getLine
    lines <- replicateM n getLine
    let ls = map readLocation lines
        ms = makeMinefield ls
    l <- readLocation <$> getLine
    let cs = sort $ revealedCells (Size w h) ms [] l
        outs = map show cs
    mapM_ putStrLn outs



----------------------------

-- I want to test 10 x 10
instance Arbitrary Location where
    arbitrary = Location <$> (choose (0, 9)) <*> (choose (0, 9))
