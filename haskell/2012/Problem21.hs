-- variable number of playing cards
-- cards :: [Card]
-- 40 minutes = 4 points! good job!

import Data.Char
import Data.List as List
import Debug.Trace
import System.IO

-- best hand not exceeding 21

type Card = Char
type Points = [Int]

bestHand :: Int -> [Card] -> [Card]
bestHand max cards = undefined

bestScore :: Int -> [Card] -> Int
bestScore max cs =
    let all = allChoices $ map points cs :: [Points]
        scores = map (bestComboScore max) all
    in maximum scores

allChoices :: [Points] -> [Points]
allChoices cps = sequence cps

sample :: [Card]
sample = "2a4k"

bestComboScore :: Int -> Points -> Int
bestComboScore max pss =
  maximum $ filter (<= max) $ map comboScore $ List.subsequences pss

comboScore :: Points -> Int
comboScore = sum

points :: Card -> Points
points 'j' = [10]
points 'q' = [10]
points 'k' = [10]
points 'a' = [1,11]
points n = [digitToInt n]

test :: FilePath -> IO ()
test p = openFile p ReadMode >>= run

run :: Handle -> IO ()
run h = do
    _ <- hGetLine h
    ls <- lines <$> hGetContents h :: IO [String]
    let css = map (map toLower) ls :: [[Card]]
        bests = map (bestScore 21) css
    mapM_ print bests

main = run stdin
