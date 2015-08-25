module Main where

import System.IO

type Clothing = String

combos :: [[String]] -> [[String]]
combos = sequence

test :: FilePath -> IO ()
test p = openFile p ReadMode >>= run

run :: Handle -> IO ()
run h = do
    ls <- lines <$> hGetContents h
    let wardrobe = map words ls
        cs = combos wardrobe
        outs = map unwords cs
        out = unlines outs
    putStr out

main = run stdin

