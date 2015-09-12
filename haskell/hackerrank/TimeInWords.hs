import System.IO
import Control.Applicative

data Time = Time
          { hours :: Int
          , minutes :: Int
          } deriving (Show, Eq)

parseTime :: String -> Time
parseTime ts =
    let (hs, rest) = span (/= ':') ts
        ms = drop 1 rest
    in Time (read hs) (read ms)

timeWords :: Time -> String
timeWords (Time h 0) = numWords h ++ " o' clock"
timeWords (Time h 15) = "quarter past " ++ numWords h
timeWords (Time h 30) = "half past " ++ numWords h
timeWords (Time h 45) = "quarter to " ++ numWords (h+1)
timeWords (Time h m)
  | m <= 30 = minutesWords m ++ " past " ++ numWords h
  | m > 30  = minutesWords (60 - m) ++ " to " ++ numWords (h+1)

numWords :: Int -> String
numWords 0 = "zero"
numWords 1 = "one"
numWords 2 = "two"
numWords 3 = "three"
numWords 4 = "four"
numWords 5 = "five"
numWords 6 = "six"
numWords 7 = "seven"
numWords 8 = "eight"
numWords 9 = "nine"
numWords 10 = "ten"
numWords 11 = "eleven"
numWords 12 = "twelve"
numWords 13 = "thirteen"
numWords 14 = "fourteen"
numWords 15 = "fifteen"
numWords 16 = "sixteen"
numWords 17 = "seventeen"
numWords 18 = "eighteen"
numWords 19 = "nineteen"
numWords 20 = "twenty"
numWords n = "twenty " ++ numWords (n - 20)

minutesWords :: Int -> String
minutesWords 1 = "one minute"
minutesWords n = numWords n ++ " minutes"

mins :: String -> String
mins ms = ms ++ " minutes"


testFile :: FilePath -> IO ()
testFile p = openFile p ReadMode >>= run

test = testFile "test.txt"

run :: Handle -> IO ()
run h = do
    c <- hGetContents h
    let [hs, ms] = lines c
        time = Time (read hs) (read ms)
        ws = timeWords time
    putStrLn ws

main = run stdin
