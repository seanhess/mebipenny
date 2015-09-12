
import Data.Ratio
import System.IO

positive = ( > 0 )
negative = ( < 0 )
zeros = ( == 0 )

totals ns = (filter positive ns, filter negative ns, filter zeros ns)

fracs :: Int -> ([a], [a], [a]) -> (Rational, Rational, Rational)
fracs len (ps, ns, zs) =
  let tot = toInteger len
  in (toInteger (length ps) % tot, toInteger (length ns) % tot, toInteger (length zs) % tot)

toOut :: Rational -> String
toOut n =
    let f = fromRational n :: Float
    in show f

testFile :: FilePath -> IO ()
testFile p = openFile p ReadMode >>= run

test = testFile "test.txt"


run :: Handle -> IO ()
run h = do
    c <- hGetContents h
    let line = last $ lines c
    let nums = map read $ words line :: [Integer]
        (ps, ns, zs) = fracs (length nums) (totals nums)
    putStrLn $ toOut ps
    putStrLn $ toOut ns
    putStrLn $ toOut zs

main = run stdin
