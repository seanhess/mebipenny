{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding (lookup)
import Control.Monad (forM_)
import Data.List (intersperse, sort, intercalate, subsequences, isPrefixOf)
import Data.HashMap.Strict (HashMap, lookup, fromList)
import Data.Hashable (Hashable)
import Data.Maybe (isJust, catMaybes)
import System.IO (Handle, hGetContents, stdin, openFile, IOMode(..), hGetLine)

type Morse = String

data CodeKey = CodeKey Char Morse
             deriving (Show, Eq)

codes =
    [ CodeKey 'A' "._"
    , CodeKey 'B' "_..."
    , CodeKey 'C' "_._."
    , CodeKey 'D' "_.."
    , CodeKey 'E' "."
    , CodeKey 'F' ".._."
    , CodeKey 'G' "__."
    , CodeKey 'H' "...."
    , CodeKey 'I' ".."
    , CodeKey 'J' ".___"
    , CodeKey 'K' "_._"
    , CodeKey 'L' "._.."
    , CodeKey 'M' "__"
    , CodeKey 'N' "_."
    , CodeKey 'O' "___"
    , CodeKey 'P' ".__."
    , CodeKey 'Q' "__._"
    , CodeKey 'R' "._."
    , CodeKey 'S' "..."
    , CodeKey 'T' "_"
    , CodeKey 'U' ".._"
    , CodeKey 'V' "..._"
    , CodeKey 'W' ".__"
    , CodeKey 'X' "_.._"
    , CodeKey 'Y' "_.__"
    , CodeKey 'Z' "__.."
    ]


output :: [String] -> String
output ss = intercalate "\n" $ sort ss

translations :: Morse -> [String]
translations morse =
    let cs = filter (matchCode morse) codes
    in concat $ map (codeTranslations morse) cs

-- if it drops to zero I need to just return the letter
codeTranslations :: Morse -> CodeKey -> [String]
codeTranslations morse (CodeKey l cs) =
    case drop (length cs) morse of
      "" -> [[l]]
      rest -> map (l:) $ translations rest

-- no I j
matchCode :: Morse -> CodeKey -> Bool
matchCode morse (CodeKey l cs) = cs `isPrefixOf` morse

sample = "__"
sample2 = ".__"
sample3 = ".__..__._..._"

-- there's probably some way to get it to go

test :: FilePath -> IO ()
test p = openFile p ReadMode >>= run

run :: Handle -> IO ()
run h = do
    codes <- lines <$> hGetContents h
    -- lines cleans it up a lot
    forM_ codes $ \code -> do
      let ts = translations code
          out = output ts
      putStrLn out

main = run stdin


