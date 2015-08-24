
module Main where

import Data.Maybe (catMaybes)
import Data.Char (intToDigit, toLower)

toNum :: Char -> Maybe Int
toNum 'a' = Just 2
toNum 'b' = Just 2
toNum 'c' = Just 2

toNum 'd' = Just 3
toNum 'e' = Just 3
toNum 'f' = Just 3

toNum 'g' = Just 4
toNum 'h' = Just 4
toNum 'i' = Just 4

toNum 'j' = Just 5
toNum 'k' = Just 5
toNum 'l' = Just 5

toNum 'm' = Just 6
toNum 'n' = Just 6
toNum 'o' = Just 6

toNum 'p' = Just 7
toNum 'q' = Just 7
toNum 'r' = Just 7
toNum 's' = Just 7

toNum 't' = Just 8
toNum 'u' = Just 8
toNum 'v' = Just 8

toNum 'w' = Just 9
toNum 'x' = Just 9
toNum 'y' = Just 9
toNum 'z' = Just 9

toNum '0' = Just 0
toNum '1' = Just 1
toNum '2' = Just 2
toNum '3' = Just 3
toNum '4' = Just 4
toNum '5' = Just 5
toNum '6' = Just 6
toNum '7' = Just 7
toNum '8' = Just 8
toNum '9' = Just 9
toNum _ = Nothing

toNums :: String -> [Int]
toNums = catMaybes . map (toNum.toLower)

intsToString :: [Int] -> String
intsToString ns = map intToDigit ns

main = do
    contents <- getContents
    let ls = lines contents
        nums = map toNums ls
        outs = map intsToString nums
    mapM_ putStrLn outs


