import Control.Monad
import Control.Applicative

main = do
    n <- read <$> getLine :: IO Int
    l <- getLine
    let ns = map read $ words l :: [Integer]
        tot = sum ns
    putStrLn (show tot)

