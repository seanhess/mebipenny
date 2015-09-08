{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Actions where

import Control.Monad.State hiding (state)
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Engine
import qualified Network.Wreq as Wreq
import Network.Wreq
import Network.HTTP.Client (HttpException, defaultManagerSettings, managerResponseTimeout)
import Control.Lens
import Data.Aeson
import Data.Text (Text)
import Network.HTTP.Types.Header (HeaderName)

import qualified Types.NewGameRequest as NewGameRequest
import Types.NewPlayerRequest
import Types.NewGameRequest
import Types.Game (Game, state, GameState(..))
import Types.GameDelta (GameDelta, applyDelta)
import qualified Types.Game as Game
import Types.Player
import Types.Move
import Types.Tile

import Control.Exception

rootUrl = "http://localhost:9292"
smallGame = NewGameRequest 4 4 2
tokenHeader = "X-Turn-Token"

second = 1000 * 1000
opts = defaults & manager .~ Left (defaultManagerSettings { managerResponseTimeout = Just (1000 * second) } )

type TurnToken = ByteString


-- I want to log the request AND the response...

logResponse :: IO (Response BSL.ByteString) -> IO (Response BSL.ByteString)
logResponse action = do
    res <- action
    let body = res ^. responseBody
        status = res ^. responseStatus . statusCode
    putStrLn $ " <== " ++ show status ++ " " ++ BSLC.unpack body
    return res


-- TODO logging (status, error codes, bodies)
-- TODO exception handling (especially for non-200 status codes, they use those a lot)
-- I could have it return an either, but really I just want to throw if it doesn't exist

-- Or I could just make all of these ExceptT MonadIO
-- then we would handle them better

createGame :: NewGameRequest -> IO String
createGame req = do
    r <- logResponse $ Wreq.post rootUrl (toJSON req)
    let l = readHeader "Location" r
    putStrLn $ "CREATED: " <> show l
    return $ BS.unpack l

joinGame :: String -> NewPlayerRequest -> IO (TurnToken, Game)
joinGame gamePath player = do
    r <- logResponse $ Wreq.postWith opts (rootUrl <> gamePath <> "/players") (toJSON player)
    let game = decodeBody r
        token = readHeader tokenHeader r
    return (token, game)

playMove :: String -> TurnToken -> Move -> IO (TurnToken, GameDelta)
playMove gamePath token move = do
    let opts' = opts & header tokenHeader .~ [token]
    r <- logResponse $ Wreq.postWith opts' (rootUrl <> gamePath <> "/moves") (toJSON move)
    let game = decodeBody r
        token = readHeader tokenHeader r
    return (token, game)

-- there has to be a function that takes an either and throws it
readHeader :: HeaderName -> Response a -> ByteString
readHeader h res =
  case res ^? responseHeader h of
    Nothing -> error ("Could not find header: " ++ show h)
    Just hd -> hd

decodeBody :: FromJSON a => Response BSL.ByteString -> a
decodeBody res =
  case eitherDecode $ res ^. responseBody of
    Left err -> error err
    Right a -> a

-----------------------------------------------------------------

-- alice needs to join first
play :: String -> String -> IO ()
play gamePath name = do
    (token, game) <- joinGame gamePath (NewPlayerRequest name)
    putStrLn ("joined" ++ name)
    print game
    let playerId = Game.player_id game
    turns gamePath playerId token game

turn :: String -> String -> TurnToken -> Game -> IO (TurnToken, Game)
turn gamePath playerId token game = do
    let tiles = Game.playerTiles playerId (Game.players game)
    case bestMove playerId tiles (gameBoard game) of
      Nothing -> do
        putStrLn "NO MOVE"
        return $ (token, game)

      Just (score, tile) -> do
        putStrLn ("BEST MOVE: " ++ show score ++ " " ++ show tile)
        (token, delta) <- playMove gamePath token (Move tile playerId)
        let game' = applyDelta delta game
        return (token, game')

match :: String -> IO ()
match name = do
    res <- logResponse $ Wreq.get (rootUrl ++ "/match")
    let gamePath = BS.unpack $ readHeader "Location" res
    putStrLn gamePath
    play gamePath name

-- this is the recursive loop so he can decide to stop!
turns :: String -> String -> TurnToken -> Game -> IO ()
turns gamePath playerId token game = do
    (token', game') <- turn gamePath playerId token game
    if state game' == Completed
      then putStrLn "COMPLETE"
      else turns gamePath playerId token' game'

-------------------------------------------------------------------------------------


data TurnState = TurnState
  { playerId :: PlayerId
  , gamePath :: String
  , turnToken :: TurnToken
  , game :: Game
  } deriving (Show, Eq)

setToken :: TurnToken -> TurnState -> TurnState
setToken tok s = s { turnToken = tok }

setGame :: Game -> TurnState -> TurnState
setGame g s = s { game = g }

newtype GameIO a = GameIO
  { unGame :: StateT TurnState IO a }
  deriving (Monad, Functor, Applicative, MonadIO, MonadState TurnState)

-- the problem is I don't know the game path yet, so I can't initialize the game state until afterward

turn' :: GameIO ()
turn' = do
    g <- gets game
    pid <- gets playerId
    let tiles = Game.playerTiles pid (Game.players g)
    case bestMove pid tiles (gameBoard g) of
      Nothing -> do
        liftIO $ putStrLn "NO MOVE"
        return ()

      Just (score, tile) -> do
        liftIO $ putStrLn ("BEST MOVE: " ++ show score ++ " " ++ show tile)
        playMove' (Move tile pid)

playMove' :: Move -> GameIO ()
playMove' move = do
    tok <- gets turnToken
    gp <- gets gamePath
    (token, game) <- liftIO $ playMove gp tok move
    saveDelta game
    saveToken token

saveDelta :: GameDelta -> GameIO ()
saveDelta delta = modify (\s -> setGame (applyDelta delta (game s)) s)

saveToken :: TurnToken -> GameIO ()
saveToken tok = modify (setToken tok)

-- would this have made more sense as State?
-- StateT ActionState IO ()
-- it feels like some kind of fold... I'm folding over and keeping going...
-- naw, a recursive loop is better...
-- but the token thing is bogus. I have to be able to improve that with state
