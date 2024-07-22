{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Blockchain.Data.Keys where

import Reflex.Dom.Core

import           Blockchain.Data.RLP
import           Blockchain.Data.Util
import           Blockchain.Data.ExtendedWord
import           Blockchain.Data.Keccak256
import           Control.Lens hiding ((.=))
import           Control.Monad ((<=<))
import qualified Crypto.PubKey.ECC.ECDSA as E
import           Crypto.PubKey.ECC.Generate
import           Crypto.PubKey.ECC.Types
import           Crypto.Random.Entropy
import           Data.Aeson
import           Data.Aeson.Key
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

data Signature = Signature {
  r :: Word256,
  s :: Word256,
  v :: Word8
} deriving (Eq, Ord, Show, Generic)

instance ToJSVal Signature where
--  toJSVal = toJSVal_generic id

instance FromJSVal Signature where
--  fromJSVal o = do
--    oVal <- val o
--    (rVal :: JSVal) <- oVal ^. js "r"
--    (sVal :: JSVal) <- oVal ^. js "s"
--    (vVal :: JSVal) <- oVal ^. js "v"
--    mR <- fromJSVal rVal
--    mS <- fromJSVal sVal
--    mV <- fromJSVal sVal
--    pure $ Signature <$> mR <*> mS <*> mV

instance ToJSON Signature where
  toJSON (Signature r s v) = object
    [ (fromString "r") .= r
    , (fromString "s") .= s
    , (fromString "v") .= v
    ]

instance FromJSON Signature where
  parseJSON (Data.Aeson.Object o) = Signature
                                <$> (o .: fromString "r")
                                <*> (o .: fromString "s")
                                <*> (o .: fromString "v")
  parseJSON o = fail $ "Error parsing Signature: Expected object, got: " <> show o

newtype PrivateKey = PrivateKey {
  unPrivateKey :: Word256
} deriving (Eq, Ord, Show, Read, Generic)

instance ToJSVal PrivateKey where
  toJSVal = toJSVal . unPrivateKey

instance FromJSVal PrivateKey where
  fromJSVal = fmap (fmap PrivateKey) . fromJSVal

data PublicKey = PublicKey {
  x :: Word256,
  y :: Word256
} deriving (Eq, Ord, Show, Generic)

instance ToJSVal PublicKey where
--  toJSVal = toJSVal_generic id

instance FromJSVal PublicKey where
  fromJSVal o = do
    oVal <- val o
    (xVal :: JSVal) <- oVal ^. js "x"
    (yVal :: JSVal) <- oVal ^. js "y"
    mX <- fromJSVal xVal
    mY <- fromJSVal yVal
    pure $ PublicKey <$> mX <*> mY

instance ToJSON PublicKey where
  toJSON = toJSON . decodeUtf8 . B16.encode . publicKeyToByteString

instance FromJSON PublicKey where
  parseJSON = either fail (pure . byteStringToPublicKey) . B16.decode . encodeUtf8 <=< parseJSON

instance RLPSerializable PublicKey where
  rlpEncode = rlpEncode . decodeUtf8 . B16.encode . publicKeyToByteString
  rlpDecode = either (error "rlpDecode: PublicKey") byteStringToPublicKey . B16.decode . encodeUtf8 . rlpDecode

publicKeyToByteString :: PublicKey -> ByteString
publicKeyToByteString (PublicKey x y) =
       BS.singleton 0x4
    <> word256ToByteString x
    <> word256ToByteString y

byteStringToPublicKey :: ByteString -> PublicKey
byteStringToPublicKey bs =
  let xy = BS.drop 1 bs
   in PublicKey
        (byteStringToWord256 $ BS.take 32 xy)
        (byteStringToWord256 $ BS.drop 32 xy)

newPrivateKey :: IO PrivateKey
newPrivateKey = PrivateKey . Word256 . E.private_d . snd <$> generate (getCurveByName SEC_p256k1)

toPublicKey :: MonadJSM m => PrivateKey -> m PublicKey
toPublicKey pk = liftJSM $ do
  s <- jsg ("Secp256k1" :: String) 
  pkBN <- s ^. js2 "uint256" pk ("hex" :: String)
  jsPub <- s ^. js1 "generatePublicKeyFromPrivateKeyData" pkBN
  fromJSValUnchecked jsPub

sign :: MonadJSM m => PrivateKey -> Keccak256 -> m Signature
sign pk msg = liftJSM $ do
  s' <- jsg ("Secp256k1" :: String) 
  pkBN <- s' ^. js2 "uint256" pk ("hex" :: String)
  let msgText = decodeUtf8 . B16.encode $ keccak256ToByteString msg
  msgBN <- s' ^. js2 "uint256" msgText ("hex" :: String)
  jsSig <- s' ^. js2 "ecsign" pkBN msgBN
  sig <- fromJSValUnchecked jsSig
  pub <- toPublicKey pk
  pub0 <- verify' 0 sig msg
  if pub == pub0
    then pure $ Signature (r sig) (s sig) 0
    else pure $ Signature (r sig) (s sig) 1

verify :: MonadJSM m => Signature -> Keccak256 -> m PublicKey
verify sig@(Signature _ _ v) = verify' v sig

verify' :: MonadJSM m => Word8 -> Signature -> Keccak256 -> m PublicKey
verify' v (Signature r s _) msg = liftJSM $ do
  s' <- jsg ("Secp256k1" :: String) 
  rBN <- s' ^. js2 "uint256" r ("hex" :: String)
  sBN <- s' ^. js2 "uint256" s ("hex" :: String)
  let msgText = decodeUtf8 . B16.encode $ keccak256ToByteString msg
  msgBN <- s' ^. js2 "uint256" msgText ("hex" :: String)
  jsPub <- s' ^. js4 "ecrecover" v rBN sBN msgBN
  fromJSValUnchecked jsPub

rlpSign :: (RLPSerializable a, MonadJSM m) => PrivateKey -> a -> m Signature
rlpSign pk = sign pk . rlpHash