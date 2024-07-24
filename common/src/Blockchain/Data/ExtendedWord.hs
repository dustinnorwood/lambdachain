{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Blockchain.Data.ExtendedWord where

import Reflex.Dom.Core

import           Blockchain.Data.RLP
import           Blockchain.Data.Util
import           Control.Lens
import           Control.Monad ((<=<))
import           Data.Aeson
import           Data.Bits ((.|.), shiftL, shiftR)
import           Data.ByteArray
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import           Data.List (unfoldr)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Word
import           GHC.Generics
import           Language.Javascript.JSaddle
import           Text.Read (readMaybe)
import Blockchain.Data.Util (byteString2Integer, integer2Bytes)

newtype Word256 = Word256 {
  word256ToInteger :: Integer
} deriving (Eq, Ord, Generic)

instance Show Word256 where
  show = C8.unpack
       . B16.encode
       . word256ToByteString

instance Read Word256 where
  readsPrec _ = either
                  (const [])
                  ( (:[])
                  . (,"")
                  . byteStringToWord256
                  )
              . B16.decode
              . C8.pack

instance ToJSVal Word256 where
  toJSVal = toJSVal . show

instance FromJSVal Word256 where
  fromJSVal v = (readMaybe =<<) <$> fromJSVal v

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
word256ToByteString = (\b -> BS.drop (BS.length b - 32) b) . (BS.replicate 32 0 <>) . integer2Bytes . word256ToInteger

newtype Word160 = Word160 {
  word160ToInteger :: Integer
} deriving (Eq, Ord, Generic)

instance Show Word160 where
  show = C8.unpack
       . B16.encode
       . word160ToByteString

instance Read Word160 where
  readsPrec _ = either
                  (const [])
                  ( (:[])
                  . (,"")
                  . byteStringToWord160
                  )
              . B16.decode
              . C8.pack

instance ToJSVal Word160 where
  toJSVal = toJSVal . show

instance FromJSVal Word160 where
  fromJSVal v = (readMaybe =<<) <$> fromJSVal v

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