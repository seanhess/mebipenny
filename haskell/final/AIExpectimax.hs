-- https://github.com/billpmurphy/h2048/blob/master/hSolve2048.hs
module AIExpectimax where

import Prelude hiding (Left, Right)
import Data.List (genericLength, sortBy)
import Data.Ord (comparing)
import System.IO (hSetBuffering, stdin, BufferMode(..))

-- import H2048 hiding (gameLoop, main)

type Probability = Double
type Utility = Double
type Distribution a = [(Probability, a)]

expectedUtility :: (a -> Utility) -> Distribution a -> Utility
expectedUtility utility = sum . map expected
    where expected x = (fst x) * (utility $ snd x)

data EXTree a = DecisionNode a [EXTree a]
              | RandomNode a (Distribution (EXTree a))
              | Leaf a
              deriving (Show, Eq)

currentState :: EXTree a -> a
currentState node = case node of
        Leaf state           -> state
        RandomNode state _   -> state
        DecisionNode state _ -> state

bestChoice :: (Eq a) => (a -> Utility) -> EXTree a -> a
bestChoice utils node = case node of
        Leaf state             -> state
        RandomNode state _     -> state
        DecisionNode state ds  -> currentState . bestTree . filtered state $ ds
    where filtered s = filter (\x -> currentState x /= s)
          bestTree   = head . reverse . sortBy (comparing $ treeUtility utils)

treeUtility :: (a -> Double) -> EXTree a -> Double
treeUtility utility node = case node of
        Leaf state        -> utility state
        RandomNode _ dist -> expectedUtility (treeUtility utility) dist
        DecisionNode _ ds -> maximum $ map (treeUtility utility) ds

buildEXTree :: (a -> Bool) -> (a -> [a]) -> (a -> Distribution a) -> Int -> a -> EXTree a
buildEXTree movesLeft allMoves allEvents depth state
    | (not $ movesLeft state) || (depth <= 0) = Leaf state
    | otherwise                               = options state
    where options s = DecisionNode s . map runEnvironment . allMoves $ s
          runEnvironment s = RandomNode s . map next . allEvents $ s
          next (p, s) = (p, buildEXTree movesLeft allMoves allEvents (depth-1) s)
