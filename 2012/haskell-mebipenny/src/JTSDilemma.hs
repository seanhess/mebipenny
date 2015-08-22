{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding (lookup)
import Data.List (intersperse, sort, intercalate, subsequences)
import Data.HashMap.Strict (HashMap, lookup, fromList)
import Data.Hashable (Hashable)
import Data.Maybe (isJust, catMaybes)

newtype MorseCode = MorseCode String
                  deriving (Eq, Show, Hashable)

morseMap :: HashMap MorseCode Char
morseMap = fromList
    [ (MorseCode "._", 'A')
    , (MorseCode "_...", 'B')
    , (MorseCode "_._.", 'C')
    , (MorseCode "_..", 'D')
    , (MorseCode ".", 'E')
    , (MorseCode ".._.", 'F')
    , (MorseCode "__.", 'G')
    , (MorseCode "....", 'H')
    , (MorseCode "..", 'I')
    , (MorseCode ".___", 'J')
    , (MorseCode "_._", 'K')
    , (MorseCode "._..", 'L')
    , (MorseCode "__", 'M')
    , (MorseCode "_.", 'N')
    , (MorseCode "___", 'O')
    , (MorseCode ".__.", 'P')
    , (MorseCode "__._", 'Q')
    , (MorseCode "._.", 'R')
    , (MorseCode "...", 'S')
    , (MorseCode "_", 'T')
    , (MorseCode ".._", 'U')
    , (MorseCode "..._", 'V')
    , (MorseCode ".__", 'W')
    , (MorseCode "_.._", 'X')
    , (MorseCode "_.__", 'Y')
    , (MorseCode "__..", 'Z')
    ]

translations :: HashMap MorseCode Char -> MorseCode -> [String]
translations codes (MorseCode code) =
    let seqs = sequences code :: [[String]]
        mcds = map (map MorseCode) seqs :: [[MorseCode]]
    in catMaybes $ map (findCodes codes) mcds

findCodes :: HashMap MorseCode Char -> [MorseCode] -> Maybe String
findCodes codes seqs = allJust $ map (findCode codes) seqs

allJust :: [Maybe a] -> Maybe [a]
allJust xs =
  if all isJust xs
    then Just $ catMaybes xs
    else Nothing

findCode :: HashMap MorseCode Char -> MorseCode -> Maybe Char
findCode codes code = lookup code codes

sequences :: String -> [[String]]
sequences cs = go cs
  where
    go :: String -> [[String]]
    go []  = []
    go [c] = [[c:""]]
    go (c:cs) =
      let subs = go cs
          run = [c]
          pres = map (prefixRun run) subs
          nexts = map (\seq -> [run] ++ seq) subs
      in pres ++ nexts

prefixRun :: String -> [String] -> [String]
prefixRun pre [] = []
prefixRun pre (x:xs) = (pre ++ x) : xs

output :: [String] -> String
output ss = intercalate "\n" $ sort ss

-- need a simple recursive function
-- if it isn't in the dictionary, proceed to the next character
-- if it is, then return it as an option and continue

sample = MorseCode "__"
sample2 = MorseCode ".__"
sample3 = MorseCode ".__..__._..._"

main = do
    code <- MorseCode <$> getLine
    putStrLn $ output $ translations morseMap code


