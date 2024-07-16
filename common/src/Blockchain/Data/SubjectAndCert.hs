{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Blockchain.Data.SubjectAndCert where

import           Control.Applicative ((<|>))
import           Blockchain.Data.RLP
import           Blockchain.Data.Util
import           Blockchain.Data.ExtendedWord
import           Blockchain.Data.Keccak256
import           Blockchain.Data.Subject
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

data SubjectAndCert = SubjectAndCert
  { sacSubject     :: Subject
  , sacCertificate :: Maybe Text
  } deriving (Eq, Show)

instance ToJSON SubjectAndCert where
  toJSON (SubjectAndCert s c) = object
    [ "subject" .= s
    , "certificate" .= c
    ]

instance FromJSON SubjectAndCert where
  parseJSON = withObject "SubjectAndCert" $ \o -> SubjectAndCert
    <$> (o .: "subject")
    <*> (o .:? "certificate")

instance RLPSerializable SubjectAndCert where
  rlpEncode (SubjectAndCert s Nothing)  = rlpEncode s
  rlpEncode (SubjectAndCert s (Just c)) = RLPArray [rlpEncode s, rlpEncode c]
  rlpDecode (RLPArray [s, c])           = SubjectAndCert (rlpDecode s) (Just $ rlpDecode c)
  rlpDecode s                           = SubjectAndCert (rlpDecode s) Nothing