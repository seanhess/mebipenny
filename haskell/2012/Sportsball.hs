module Main where

import System.IO
import Data.List
import Data.Monoid

test p = openFile p ReadMode >>= run

run :: Handle -> IO ()
run h = do
    ls <- lines <$> hGetContents h
    let runs = map (map read . words) ls :: [[Int]]
        bests = map bestRun runs
    mapM_ print bests

main = run stdin

-- point margins: between the teams?, 16 -3 -2 -4 -1 5  23 11 1
-- find odd margin streaks: -1 5 23 11 1
-- total points 

-- consecutive odd integers in each list
-- sum

bothOdd a b = odd a && odd b

allOdd :: [Int] -> Bool
allOdd = all odd

consecutiveOdds :: [Int] -> [[Int]]
consecutiveOdds ns = filter allOdd $ groupBy bothOdd ns

bestRun :: [Int] -> Int
bestRun ns =
    case concatMap (map sum) $ map subRuns $ consecutiveOdds ns of
      [] -> 0
      nss -> maximum nss

subRuns :: [Int] -> [[Int]]
subRuns ns = concat $ map tails $ (drop 1 (inits ns))

sample3 :: [Int]
sample3 = [1, 43, -31, 17, 23, -45, -42, 32, -2, -28]

sample4 :: [Int]
sample4 = [-6, 6, 38, -37, 17, 101, 1, -85, 23, -62]

-- odd function
-- it's not sum of the largest list
-- it's the largest sum
-- so you don't have to use all of it

