module Main where

import Control.Applicative
import Data.List

staircase h = unlines $ stairRows h

stairRows :: Int -> [String]
stairRows height = map (stairRow height) [1..height]

stairRow :: Int -> Int -> String
stairRow height n = replicate (height-n) ' ' ++ replicate n '#'

main = do
    n <- read <$> getLine
    putStrLn $ staircase n
