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

module Blockchain.Data.Subject where

import           Control.Applicative ((<|>))
import           Blockchain.Data.RLP
import           Blockchain.Data.Util
import           Blockchain.Data.ExtendedWord
import           Blockchain.Data.Keccak256
import           Blockchain.Data.Keys
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

data Subject = Subject
  { subCommonName :: String,
    subOrg :: String,
    subUnit :: Maybe String,
    subCountry :: Maybe String,
    subPub :: PublicKey
  }
  deriving (Show, Eq, Generic)

-- issuerEqSubject :: Issuer -> Subject -> Bool
-- issuerEqSubject Issuer {..} Subject {..} =
--   (issCommonName, issOrg, issUnit, issCountry) == (subCommonName, subOrg, subUnit, subCountry)

instance ToJSON Subject where
  toJSON (Subject cn o ou c pub) =
    object
      [ "commonName" .= cn,
        "organization" .= o,
        "organizationUnit" .= ou,
        "country" .= c,
        "pubKey" .= pub
      ]

instance FromJSON Subject where
  parseJSON (Object obj) = do
    cn <- obj .: "commonName"
    o <- obj .: "organization"
    ou <- obj .:? "organizationUnit"
    c <- obj .:? "country"
    pub <- obj .: "pubKey"
    return $ Subject cn o ou c pub
  parseJSON x = fail $ "could not decode JSON subject info: " ++ show x
instance RLPSerializable Subject where
  rlpEncode (Subject cn o u c p) = RLPArray [
    rlpEncode cn,
    rlpEncode o,
    rlpEncode u,
    rlpEncode c,
    rlpEncode p
    ]

  rlpDecode (RLPArray [cn, o, u, c, p]) = Subject
    (rlpDecode cn)
    (rlpDecode o)
    (rlpDecode u)
    (rlpDecode c)
    (rlpDecode p)
  rlpDecode x = error $ "rlpDecode for Subject failed: expected RLPArray of 5 elements, got " ++ show x