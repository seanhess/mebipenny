import System.IO

testFile :: FilePath -> IO ()
testFile p = openFile p ReadMode >>= run

test = testFile "test.txt"

run :: Handle -> IO ()
run h = do
    undefined
    -- hGetContents, hGetLine, etc
    -- codes <- lines <$> hGetContents h

main = run stdin
