{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blockchain.Data.TransactionResult where

import Data.Aeson
import GHC.Generics

newtype TransactionResult = TransactionResult { success :: Bool }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON TransactionResult where
  toJSON tr = object
    [ "status" .= if success tr then "success" else "failure"
    ]

instance FromJSON TransactionResult where
  parseJSON (Data.Aeson.Object o) =
    TransactionResult . (== "success") <$> o .: "status"
  parseJSON o = fail $ "parseJSON TransactionResult: Expected Object, got: " ++ show o
