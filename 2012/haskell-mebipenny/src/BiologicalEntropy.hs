{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Control.Monad (when)
import Control.Monad.State (State, get, gets, modify, runState, put)
import Data.Maybe (catMaybes)
import Data.HashMap.Strict (HashMap, fromList)
import Data.Hashable (Hashable(..))
import Data.List (maximumBy, sortOn)
import qualified Data.HashMap.Strict as HM

-- A = 0
-- G = 1
-- C = 2
-- T = 3

-- I need some kind of "next available" code, too

data DNA = A | G | C | T deriving (Eq, Show)
type Code = Int

instance Hashable DNA where
    hashWithSalt salt d = hashWithSalt salt (showDNA d)

readDNA :: Char -> DNA
readDNA 'A' = A
readDNA 'G' = G
readDNA 'C' = C
readDNA 'T' = T

showDNA :: DNA -> Char
showDNA A = 'A'
showDNA G = 'G'
showDNA C = 'C'
showDNA T = 'T'

type CodeMap = HashMap [DNA] Code

-- wait this isn't codes -> strings
-- it's strings -> codes
emptyCodes :: CodeMap
emptyCodes =
    [ ([A], 0)
    , ([G], 1)
    , ([C], 2)
    , ([T], 3)
    ]

data EncodeState
    = EncodeState
    { codes :: HashMap [DNA] Code
    , running :: [DNA]
    , maxCode :: Code
    , currCode :: Code
    } deriving (Show)

-- the next starting code starts at 4
emptyState :: Int -> EncodeState
emptyState bits = EncodeState emptyCodes [] (bitsMax bits) 4

-- the maximum number that curr can be
bitsMax :: Int -> Code
bitsMax n = (2 ^ n) - 1

setMaxCode code st = st { maxCode = code }
setCurrCode code st = st { currCode = code }
setRunning ds st = st { running = ds }
setCodes cmap st = st { codes = cmap }

-- if set+char is in our coding map, add to set and continue
-- else 
  -- if there are available codes, map the new sequence to the next available code
  -- output the code for the SET without the new character
  -- continue processing the new character as the running sequence

dnaCode :: DNA -> Code
dnaCode A = 0
dnaCode G = 1
dnaCode C = 2
dnaCode T = 3

nextCode :: Code -> [DNA] -> Code
nextCode _    [x] = dnaCode x
nextCode curr _   = curr

--mapToCode :: Code -> [DNA] -> HashMap [DNA] Code -> (Code, HashMap [DNA] Code)
--mapToCode c ds cmap =
    --let c' = nextAvailableCode c ds
        --cmap' = HM.insert ds c' cmap
    --in (c', cmap')

addCodeToMap :: [DNA] -> State EncodeState ()
addCodeToMap ds = do
    curr <- gets currCode
    cmap <- gets codes
    let cmap' = HM.insert ds (nextCode curr ds) cmap
    modify (setCurrCode (curr+1))
    modify (setCodes cmap')

currentCode :: [DNA] -> State EncodeState (Maybe Code)
currentCode ds = do
    cmap <- gets codes
    return $ HM.lookup ds cmap

-- needs to return
-- let's store it in state
-- wait, how do I "add to set" and continue
-- I don't have a way of doing that.
-- put the running set into state
encodeChar :: DNA -> State EncodeState (Maybe Code)
encodeChar d = do
    EncodeState cmap oldseq max curr <- get
    let newseq = d : oldseq
    case HM.lookup newseq cmap of

      -- if it is already in the coding map, add new character to running set and continue
      Just _ -> do
        put (EncodeState cmap newseq max curr)
        return Nothing

      -- if it doesn't already have a code
      Nothing -> do
        -- if there are available codes, map the new sequence to the next one
        -- curr starts at 0
        -- max starts at 3
        when (curr <= max) $ addCodeToMap newseq

        -- running sequence is now just the new character
        modify (setRunning [d])

        -- output the code for the running sequence without the new character
        currentCode oldseq

encodeSequence :: [DNA] -> State EncodeState [Maybe Code]
encodeSequence ds = do
    mcs <- mapM encodeChar ds
    run <- gets running
    last <- currentCode run
    return $ mcs ++ [last]

-----------------------------------------------------------------------

-- State (HashMap Code String) 
-- it doesn't need the state really :)
bitsCodesSequence :: [DNA] -> Int -> [Code]
bitsCodesSequence ds bits =
    let (mcs, state) = runState (encodeSequence ds) (emptyState bits)
    in catMaybes mcs

allCodesSequence :: [DNA] -> [[Code]]
allCodesSequence ds = fmap (bitsCodesSequence ds) allBits

-- I need to find the FIRST one which is the smallest
-- from left to right
-- what kind of a recursion is that?
bitScores :: [DNA] -> [(Int, Int)]
bitScores ds = zip allBits $ zipWith encodingLength allBits (allCodesSequence ds)

-- you need to multiple the length by the number of bits!
encodingLength :: Int -> [Code] -> Int
encodingLength bits codes = bits * length codes

bestBit :: [(Int, Int)] -> Int
bestBit [] = 0
bestBit scores = fst $ head $ sortOn snd scores

allBits = [2,4,8,16,32]

sample :: [DNA]
sample = [A,G,A,G,A,G,A,G,A,G,A,G,A,G,A,G]

sample2 :: [DNA]
sample2 = [A,A,A,A,A,A,A,C,C,C,C,C,C,C,G,G,G,G,G,G,G,T,T,T,T,T,T,T,A,A,A,A,A,A,A,C,C,C,C,C,C,C]

toDNA :: String -> [DNA]
toDNA = map readDNA

main :: IO ()
main = do
    -- while we have input, read it all
    -- lines, readContent
    contents <- getContents
    let ls = lines contents
        dss = map toDNA ls
        bits = map (bestBit . bitScores) dss
    mapM_ print bits

-- so, do the algorithm with a fixed / max size, get the encoding
-- check the length produced by various encodings

