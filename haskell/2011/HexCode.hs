-- 2:48 PM
module Main where

import Data.List
import Debug.Trace

main = putStrLn "ASDF"

codes =
  [ Code 'a' "1"
  , Code 'b' "2"
  , Code 'c' "3"
  , Code 'd' "4"
  , Code 'e' "5"
  , Code 'f' "6"
  , Code 'g' "7"
  , Code 'h' "8"
  , Code 'i' "9"
  , Code 'j' "A"
  , Code 'k' "B"
  , Code 'l' "C"
  , Code 'm' "D"
  , Code 'n' "E"
  , Code 'o' "F"
  , Code 'p' "10"
  , Code 'q' "11"
  , Code 'r' "12"
  , Code 's' "13"
  , Code 't' "14"
  , Code 'u' "15"
  , Code 'v' "16"
  , Code 'w' "17"
  , Code 'x' "18"
  , Code 'y' "19"
  , Code 'z' "1A"
  ]

data Code = Code
  { char :: Char
  , code :: String
  } deriving (Show, Eq)

combos :: [Char] -> [String]
combos str =
  let cs = match str
  in concatMap (combosNext str) cs

combosNext :: String -> Code -> [String]
combosNext str (Code c pre) =
  case drop (length pre) str of
    "" -> [[c]]
    rest -> map (c:) (combos rest)

dropPrefix :: String -> Code -> String
dropPrefix str (Code c cd) = drop (length cd) str



  -- but i need to prepend it at least once!
  -- in map (\(c, s) -> sequence ([char c] ++ combos s) ) nexts

  -- in concatMap (each str) cs
  -- map (c:) $ combos $ drop (length cd) str

-- each [] (Code c cd) = [[c]]
-- each str (Code c cd) =

-- match the prefix... 
matching :: [Code] -> String -> [Code]
matching codes str = filter (\c -> code c `isPrefixOf` str) codes

match = matching codes



-- hanshotfirst
-- string: 0-9 A-F

-- how many possible messages could have generated said string
