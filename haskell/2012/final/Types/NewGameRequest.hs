{-# LANGUAGE DeriveGeneric #-}
module Types.NewGameRequest where

import Data.Aeson
import GHC.Generics

data NewGameRequest = NewGameRequest
                    { rows :: Int
                    , cols :: Int
                    , seats :: Int
                    } deriving (Show, Eq, Generic)

instance ToJSON NewGameRequest
