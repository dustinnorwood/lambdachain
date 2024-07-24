{-# LANGUAGE DerivingStrategies #-}
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

import           Blockchain.Data.Address
import           Blockchain.Data.RLP
import           Blockchain.Data.Util
import           Blockchain.Data.ExtendedWord
import           Blockchain.Data.Keccak256
import           Control.Applicative ((<|>))
import           Control.Lens hiding ((.=))
import           Control.Monad ((<=<))
import           Data.Aeson
import           Data.Aeson.Key
import           Data.Bits ((.|.), shiftL, shiftR, xor)
import           Data.ByteArray hiding (xor)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Function (on)
import           Data.List (unfoldr)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Word
import           GHC.Generics
import           Language.Javascript.JSaddle
import           Text.Read

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

instance ToJSON PrivateKey where
  toJSON (PrivateKey p) = toJSON p

instance FromJSON PrivateKey where
  parseJSON = fmap PrivateKey . parseJSON

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

data KeyInfo = KeyInfo
  { keyInfoKey     :: Keccak256
  , keyInfoAddress :: Address
  } deriving stock (Eq, Show, Generic)

instance ToJSON KeyInfo where
  toJSON (KeyInfo k a) = object
    [ (fromString "key") .= k
    , (fromString "address") .= a
    ]

instance FromJSON KeyInfo where
  parseJSON = withObject "KeyInfo" $ \o ->
    KeyInfo <$> o .: (fromString "key")
            <*> o .: (fromString "address")

data LoginInfo = LoginInfo
  { loginInfoUsername :: Text
  , loginInfoKey      :: PrivateKey
  } deriving stock (Eq, Show, Generic)

instance ToJSON LoginInfo where
  toJSON (LoginInfo u k) = object
    [ (fromString "username") .= u
    , (fromString "key") .= k
    ]

instance FromJSON LoginInfo where
  parseJSON = withObject "LoginInfo" $ \o ->
    LoginInfo <$> o .: (fromString "username")
              <*> o .: (fromString "key")

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

newPrivateKey :: MonadJSM m => m PrivateKey
newPrivateKey = do
  mPK <- liftJSM $ do
    s <- jsg ("Secp256k1" :: String) 
    pkBN <- s ^. js0 "newPrivateKey"
    -- c <- jsg ("console" :: String) 
    (pure . PrivateKey <=< readMaybe <=< readMaybe <=< id) <$> fromJSVal pkBN
    -- _ <- c ^. js1 "log" mPK
    -- mPKText <- fromJSVal pkBN
    -- _ <- c ^. js1 "log" mPKText
    -- let mPK' = either (const Nothing) Just . B16.decode . encodeUtf8 . read =<< mPKText
    -- let mPK'' = byteStringToWord256 <$> mPK'
    -- let mPK''' = PrivateKey <$> mPK''

    -- _ <- c ^. js1 "log" (decodeUtf8 . B16.encode <$> mPK')
    -- _ <- c ^. js1 "log" mPK''
    -- _ <- c ^. js1 "log" mPK'''
  case mPK of
    Just pk -> pure pk
    Nothing -> newPrivateKey

encryptPrivateKey :: Text -> PrivateKey -> ByteString
encryptPrivateKey pw (PrivateKey pk) =
  let pwBS = keccak256ToByteString . hashMsg $ encodeUtf8 pw
      pkBS = word256ToByteString pk
   in BS.pack $ (zipWith xor `on` BS.unpack) pwBS pkBS

decryptPrivateKey :: Text -> ByteString -> PrivateKey
decryptPrivateKey pw encPk =
  let pwBS = keccak256ToByteString . hashMsg $ encodeUtf8 pw
      pkBS = BS.pack $ (zipWith xor `on` BS.unpack) pwBS encPk
   in PrivateKey $ byteStringToWord256 pkBS

toPublicKey :: MonadJSM m => PrivateKey -> m PublicKey
toPublicKey pk = liftJSM $ do
  s <- jsg ("Secp256k1" :: String) 
  pkBN <- s ^. js2 "uint256" pk ("hex" :: String)
  jsPub <- s ^. js1 "generatePublicKeyFromPrivateKeyData" pkBN
  c <- jsg ("console" :: String) 
  _ <- c ^. js1 "log" pkBN
  _ <- c ^. js1 "log" jsPub
  fromJSValUnchecked jsPub

publicKeyToAddress :: PublicKey -> Address
publicKeyToAddress = Address
                   . byteStringToWord160
                   . BS.drop 12
                   . keccak256ToByteString
                   . hashMsg
                   . BS.drop 1
                   . publicKeyToByteString

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
    else do
      pub1 <- verify' 1 sig msg
      if pub == pub1
        then pure $ Signature (r sig) (s sig) 1
        else error $ "Could not verify signature: " ++ show pub ++ ", " ++ show sig

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