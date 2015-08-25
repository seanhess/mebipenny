-- fractions
-- always 1/X
-- sum of other fractions, only use denominator once

-- use the largest unit possible
-- display the components in decending order

-- let's get a list of fractions

import Data.Ratio
import System.IO

fractions :: [Rational]
fractions = map (1 %) denominators

denominators :: [Integer]
denominators = [1..]

-- I basically have a rational value
-- I want to sub
egyptian :: Rational -> [Rational]
egyptian r = go fractions r
  where
    go (f:fs) curr
      | f == curr = [f]
      | f < curr  = f : go fs (curr - f)
      | otherwise = go fs curr

test :: FilePath -> IO ()
test p = openFile p ReadMode >>= run

handleLine :: String -> IO ()
handleLine l = do
    let [n, d] = map read $ words l
        r = n % d
        fs = egyptian r
        ds = map denominator fs
        out = unwords (map show ds)
    putStrLn out

run :: Handle -> IO ()
run h = do
    ls <- lines <$> hGetContents h
    mapM_ handleLine ls
    -- hGetContents, hGetLine, etc
    -- codes <- lines <$> hGetContents h

main = run stdin
