{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Blockchain.Data.AddressState where

import           Blockchain.Data.ExtendedWord
import           Blockchain.Data.Keccak256
import           Blockchain.Data.Keys
import           Blockchain.Data.RLP
import           Common.DeriveToJSVal
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Generics

data AddressState = AddressState {
  nonce :: Integer,
  balance :: Integer -- we'll add the rest later
} deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON AddressState where
  toJSON as = object
    [ "nonce" .= nonce as
    , "balance" .= show (balance as)
    ]

instance FromJSON AddressState where
  parseJSON (Data.Aeson.Object o) = AddressState
                                <$> o .: "nonce"
                                <*> fmap read (o .: "balance")
  parseJSON o = fail $ "parseJSON AddressState: " ++ show o

blankAddressState :: AddressState
blankAddressState = AddressState 0 0