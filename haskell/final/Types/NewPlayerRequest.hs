{-# LANGUAGE DeriveGeneric #-}
module Types.NewPlayerRequest where

import Data.Aeson
import GHC.Generics

data NewPlayerRequest = NewPlayerRequest
                      { name :: String }
                      deriving (Show, Eq, Generic)

instance ToJSON NewPlayerRequest
