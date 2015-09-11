{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DisambiguateRecordFields #-}


module Main where

-- import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Solve
import qualified Network.Wreq as Wreq
import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Text

import qualified Types.NewGameRequest as NewGameRequest
import Types.NewPlayerRequest
import Types.NewGameRequest
import Types.Game
import Types.Player

import Control.Exception
import System.Environment
import Actions

-- test :: IO ()
-- test = do
    -- -- r <- Wreq.get "http://google.com"
    -- -- let code = r ^. responseStatus . statusCode
    -- -- print code

    -- let req = NewGameRequest 4 4 2
    -- r <- Wreq.post "http://localhost:9292/" (toJSON req)
    -- let body = r ^. responseBody
        -- game = eitherDecode (r ^. responseBody) :: Either String Game
    -- print (toJSON req)
    -- print body
    -- print game

main = do
  args <- getArgs
  case args of
    "play":game:name:[] -> play game name
    "create":r:c:[] -> do
      createGame (NewGameRequest (read r) (read c) 2)
      return ()

    _ -> putStrLn "Usage: join /20303 alice or create 4 4"

