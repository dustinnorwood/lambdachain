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

module Blockchain.Data.Signed where

import           Blockchain.Data.RLP
import           Blockchain.Data.Util
import           Blockchain.Data.ExtendedWord
import           Blockchain.Data.Keccak256
import           Blockchain.Data.Keys
import           Common.DeriveToJSVal
import           Data.Aeson
import           Data.ByteArray
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.List (unfoldr)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Word
import           GHC.Generics
import           Language.Javascript.JSaddle

data Signed a = Signed
  { unsigned :: a,
    signature :: Signature
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (Signed a) where
  parseJSON (Data.Aeson.Object o) = Signed
                                <$> (o .: "data")
                                <*> (o .: "signature")
  parseJSON x = fail $ "couldn't parse JSON for signed data: " ++ show x

instance ToJSON a => ToJSON (Signed a) where
  toJSON (Signed d s) =
    object
      [ "data" .= d,
        "signature" .= s
      ]