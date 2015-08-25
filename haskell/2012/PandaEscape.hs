module Main where

import Data.List
import System.IO

-- exactly N pounds
-- how many guards can go with her
-- if multiple solutions with the same number of guards, use the weight of the heaviest guard as the tie breaker, or second heaviest, etc

-- input: each line is 2 or more integers.
-- 1st is capacity of the cloud
-- 2nd is the weight of the empress
-- the remainder are the weights of the guards. 
-- up to 256 guards
-- weights of guards are between 1-255
-- max weight is 32767

-- output: optimal list of guards which will accompany, lightest to heaviest
-- NO SOLUTION if there is no solution

-- guards with weight!
type Guard = Int
type Empress = Int
type Cloud = Int

-- find: all combinations that add to exactly cloud pounds
-- subsequences!

comboWeight :: Empress -> [Guard] -> Int
comboWeight e gs = e + sum gs

validCombos :: Cloud -> Empress -> [Guard] -> [[Guard]]
validCombos c e gs = map (sortBy (flip compare)) $ filter (perfectWeight c e) $ subsequences gs

perfectWeight :: Cloud -> Empress -> [Guard] -> Bool
perfectWeight c e gs = comboWeight e gs == c

-- but what if it is empty!
bestSolution :: [[Guard]] -> Maybe [Guard]
bestSolution [] = Nothing
bestSolution gss = Just $ head $ sortBy (flip compareCombos) gss

-- assume they come pre-sorted, lightest to heaviest
compareCombos :: [Guard] -> [Guard] -> Ordering
compareCombos a b
  | length a > length b = GT
  | length a < length b = LT
  | otherwise = compareHeaviest a b

-- their length is equal
-- how do I do this?
compareHeaviest :: [Guard] -> [Guard] -> Ordering
compareHeaviest (a:as) (b:bs)
  | a > b = GT
  | a < b = LT
  | otherwise = compareHeaviest as bs

compareHeaviest _ _ = EQ


-- the thing with subsequences
-- it calculates a lot of stuff that clearly won't work
-- like, weights WAY beyond the max

test :: FilePath -> IO ()
test p = openFile p ReadMode >>= run

runLine :: String -> IO ()
runLine l = do
    let (c:e:gs) = map read $ words l :: [Int]
    print $ length $ subsequences gs

    --let sol = bestSolution $ validCombos c e gs
    --case sol of
      --Nothing -> putStrLn "NO SOLUTION"
      --Just gs -> do
        --let out = unwords (map show gs)
        --putStrLn out

run :: Handle -> IO ()
run h = do
    ls <- lines <$> hGetContents h
    mapM_ runLine ls
    -- hGetContents, hGetLine, etc
    -- codes <- lines <$> hGetContents h

main = run stdin
