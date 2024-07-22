{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blockchain.Data.TransactionResult where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data TransactionResult = TransactionResult
  { success :: Bool
  , response :: Text
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON TransactionResult where
  toJSON tr = object
    [ "status" .= if success tr then ("success" :: String) else ("failure" :: String)
    , "response" .= response tr
    ]

instance FromJSON TransactionResult where
  parseJSON (Data.Aeson.Object o) =
    TransactionResult <$> ((== ("Success!" :: String)) <$> o .: "message")
                      <*> (o .: "response")
  parseJSON o = fail $ "parseJSON TransactionResult: Expected Object, got: " ++ show o
