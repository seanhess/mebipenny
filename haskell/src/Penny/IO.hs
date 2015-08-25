import System.IO

test :: FilePath -> IO ()
test p = openFile p ReadMode >>= run

run :: Handle -> IO ()
run h = do
    undefined
    -- hGetContents, hGetLine, etc
    -- codes <- lines <$> hGetContents h

main = run stdin
