{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Blockchain.Data.Keccak256 where

import           Blockchain.Data.RLP
import           Blockchain.Data.Util
import           Blockchain.Data.ExtendedWord
import           Crypto.Hash
import           Data.Aeson
import           Data.ByteArray
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Generics
import           Language.Javascript.JSaddle

newtype Keccak256 = Keccak256 {
  keccak256ToByteString :: ByteString
} deriving (Eq, Ord, Generic)

instance Show Keccak256 where
  show = T.unpack
       . decodeUtf8
       . B16.encode
       . keccak256ToByteString

instance Read Keccak256 where
  readsPrec _ = either
                  (const [])
                  ( (:[])
                  . (,"")
                  . Keccak256
                  )
              . B16.decode
              . C8.pack

instance ToJSVal Keccak256 where
  toJSVal = toJSVal . byteStringToWord256 . keccak256ToByteString

instance FromJSVal Keccak256 where
  fromJSVal = fmap (fmap $ Keccak256 . word256ToByteString) . fromJSVal

instance ToJSON Keccak256 where
  toJSON = toJSON . byteStringToWord256 . keccak256ToByteString

instance FromJSON Keccak256 where
  parseJSON = fmap (Keccak256 . word256ToByteString) . parseJSON

emptyHash :: Keccak256
emptyHash = hashMsg ""
{-# NOINLINE emptyHash #-}

hashMsg :: ByteString -> Keccak256
hashMsg = Keccak256
        . convert
        . hashWith Keccak_256

rlpHash :: RLPSerializable a => a -> Keccak256
rlpHash = hashMsg . rlpSerialize . rlpEncode
