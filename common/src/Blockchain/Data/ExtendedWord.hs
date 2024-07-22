{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Blockchain.Data.ExtendedWord where

import Reflex.Dom.Core

import           Blockchain.Data.RLP
import           Blockchain.Data.Util
import           Control.Lens
import           Control.Monad ((<=<))
import           Crypto.Hash
import qualified Crypto.PubKey.ECC.ECDSA as E
import           Crypto.PubKey.ECC.Generate
import           Crypto.PubKey.ECC.Types
import           Crypto.Random.Entropy
import           Data.Aeson
import           Data.Bits ((.|.), shiftL, shiftR)
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
import Blockchain.Data.Util (byteString2Integer, integer2Bytes)

newtype Word256 = Word256 {
  word256ToInteger :: Integer
} deriving (Eq, Ord, Show, Read, Generic)

instance ToJSVal Word256 where
  toJSVal = toJSVal
          . decodeUtf8
          . B16.encode
          . integer2Bytes
          . word256ToInteger

instance FromJSVal Word256 where
  fromJSVal v = do
    mText <- fromJSVal v
    case mText of
      Nothing -> pure Nothing
      Just t -> pure . fmap (Word256 . byteString2Integer)
                     . either (const Nothing) Just
                     . B16.decode
                     $ encodeUtf8 t

instance ToJSON Word256 where
  toJSON = toJSON
         . decodeUtf8
         . B16.encode
         . integer2Bytes
         . word256ToInteger

instance FromJSON Word256 where
  parseJSON = fmap (Word256 . byteString2Integer)
            . either fail pure
            . B16.decode
            . encodeUtf8
            <=< parseJSON

instance RLPSerializable Word256 where
  rlpEncode = rlpEncode . word256ToInteger
  rlpDecode = Word256 . rlpDecode

byteStringToWord256 :: ByteString -> Word256
byteStringToWord256 = Word256 . byteString2Integer . BS.take 32

word256ToByteString :: Word256 -> ByteString
word256ToByteString = BS.take 32 . (<> BS.replicate 32 0) . integer2Bytes . word256ToInteger

newtype Word160 = Word160 {
  word160ToInteger :: Integer
} deriving (Eq, Ord, Show, Read, Generic)

instance ToJSVal Word160 where
  toJSVal = toJSVal
          . decodeUtf8
          . B16.encode
          . integer2Bytes
          . word160ToInteger

instance FromJSVal Word160 where
  fromJSVal v = do
    mText <- fromJSVal v
    case mText of
      Nothing -> pure Nothing
      Just t -> pure . fmap (Word160 . byteString2Integer)
                     . either (const Nothing) Just
                     . B16.decode
                     $ encodeUtf8 t

instance ToJSON Word160 where
  toJSON = toJSON
         . decodeUtf8
         . B16.encode
         . integer2Bytes
         . word160ToInteger

instance FromJSON Word160 where
  parseJSON = fmap (Word160 . byteString2Integer)
            . either fail pure
            . B16.decode
            . encodeUtf8
            <=< parseJSON

instance RLPSerializable Word160 where
  rlpEncode = rlpEncode . word160ToInteger
  rlpDecode = Word160 . rlpDecode

byteStringToWord160 :: ByteString -> Word160
byteStringToWord160 = Word160 . byteString2Integer . BS.take 20

word160ToByteString :: Word160 -> ByteString
word160ToByteString = (\b -> BS.drop (BS.length b - 20) b) . (BS.replicate 20 0 <>) . integer2Bytes . word160ToInteger