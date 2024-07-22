{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Blockchain.Data.Address where

import           Blockchain.Data.ExtendedWord
import           Blockchain.Data.Keccak256
import           Blockchain.Data.Keys
import           Blockchain.Data.RLP
import           Data.Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import           Data.Text (Text)
import qualified Data.Text as T
import           Frontend.DeriveToJSVal
import           GHC.Generics
import           Language.Javascript.JSaddle
import           Obelisk.Route
import           Text.Read

newtype Address = Address {
  addressToWord160 :: Word160
} deriving stock (Eq, Ord, Generic)

instance Show Address where
  show = C8.unpack
       . B16.encode
       . word160ToByteString
       . addressToWord160

instance Read Address where
  readsPrec _ = either
                  (const [])
                  ( (:[])
                  . (,"")
                  . Address
                  . byteStringToWord160
                  )
              . B16.decode
              . C8.pack

instance ToJSVal Address where
  toJSVal = toJSVal . addressToWord160

instance FromJSVal Address where
  fromJSVal = fmap (fmap Address) . fromJSVal

instance ToJSON Address where
  toJSON = toJSON . addressToWord160

instance FromJSON Address where
  parseJSON = fmap Address . parseJSON

instance RLPSerializable Address where
  rlpEncode = rlpEncode . addressToWord160
  rlpDecode = Address . rlpDecode

zeroAddress :: Address
zeroAddress = read "0000000000000000000000000000000000000000"
{-# NOINLINE zeroAddress #-}

publicKeyToAddress :: PublicKey -> Address
publicKeyToAddress = Address
                   . byteStringToWord160
                   . BS.drop 12
                   . keccak256ToByteString
                   . hashMsg
                   . BS.drop 1
                   . publicKeyToByteString

addressEncoder :: Applicative check => Encoder check (Either Text) Address Text
addressEncoder = unsafeMkEncoder $ EncoderImpl
  (first T.pack . readEither . T.unpack)
  (T.pack . show)