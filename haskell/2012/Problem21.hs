-- variable number of playing cards
-- cards :: [Card]
-- 40 minutes = 4 points! good job!

import Data.Char
import Data.List as List
import Debug.Trace
import System.IO

-- best hand not exceeding 21

type Card = Char

bestHand :: Int -> [Card] -> [Card]
bestHand max cards = undefined

data Points = Points Int | Choice Int Int deriving (Show, Eq)
data CardPoints = CardPoints Card Points deriving (Show, Eq)

isChoice :: Points -> Bool
isChoice (Choice _ _) = True
isChoice _ = False

getPoints :: CardPoints -> Points
getPoints (CardPoints _ p) = p

allCombos :: [Card] -> [[CardPoints]]
allCombos cards = List.subsequences (map cardPoints cards)

-- gives you the maximum possible score under the limit
-- for each choice I need to make a separate list?
-- each choice brances the possibilities

bestScore :: Int -> [CardPoints] -> Int
bestScore max cps =
    let all = allChoices cps
        scores = map (bestComboScore max) all
    in maximum scores

bestScore' :: Int -> [Card] -> Int
bestScore' max cs = bestScore max $ map cardPoints cs

allChoices :: [CardPoints] -> [[CardPoints]]
allChoices cps =
    let (choices, rest) = partition (isChoice . getPoints) cps in
    case choices of
      [] -> [cps]
      (ch:chs') ->
        let (CardPoints c (Choice l r)) = ch
            other = allChoices (chs' ++ rest)
            left = CardPoints c (Points l)
            right = CardPoints c (Points r)
        in map (left:) other ++ map (right:) other

sample :: [CardPoints]
sample = map cardPoints "2a4k"

bestComboScore :: Int -> [CardPoints] -> Int
bestComboScore max cps =
  maximum $ filter (<= max) $ map comboScore $ List.subsequences cps

comboScore :: [CardPoints] -> Int
comboScore cps = sum (map score cps)
  where
    score (CardPoints _ (Points n)) = n

cardPoints :: Card -> CardPoints
cardPoints c = CardPoints c (points c)

points :: Card -> Points
points 'j' = Points 10
points 'q' = Points 10
points 'k' = Points 10
points 'a' = Choice 1 11
points n = Points (digitToInt n)

test :: FilePath -> IO ()
test p = openFile p ReadMode >>= run

run :: Handle -> IO ()
run h = do
    _ <- hGetLine h
    ls <- lines <$> hGetContents h :: IO [String]
    let css = map (map toLower) ls :: [[Card]]
        bests = map (bestScore' 21) css
    mapM_ print bests

main = run stdin
