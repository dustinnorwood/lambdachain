{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blockchain.Data.Transaction
  ( Transaction (..),
    isMessageTX,
    partialRLPEncode,
    partialRLPDecode,
  )
where

import Blockchain.Data.Address
import Blockchain.Data.ExtendedWord
import Blockchain.Data.RLP
import Blockchain.Data.Util
import Control.Arrow ((***))
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word
import GHC.Generics
import Numeric

data Transaction
  = MessageTX
      { transactionNonce :: Integer,
        transactionGasPrice :: Integer,
        transactionGasLimit :: Integer,
        transactionTo :: Address,
        transactionValue :: Integer,
        transactionData :: B.ByteString,
        transactionChainId :: Maybe Word256,
        transactionR :: Word256,
        transactionS :: Word256,
        transactionV :: Word8,
        transactionMetadata :: Maybe (Map Text Text)
      }
  | ContractCreationTX
      { transactionNonce :: Integer,
        transactionGasPrice :: Integer,
        transactionGasLimit :: Integer,
        transactionValue :: Integer,
        transactionInit :: B.ByteString,
        transactionChainId :: Maybe Word256,
        transactionR :: Word256,
        transactionS :: Word256,
        transactionV :: Word8,
        transactionMetadata :: Maybe (Map Text Text)
      }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON Transaction where
  toJSON (MessageTX tnon tgp tgl tto tval td tcid tr ts tv md) =
    object $
      [ "nonce" .= tnon,
        "gasPrice" .= tgp,
        "gasLimit" .= tgl,
        "to" .= tto,
        "value" .= show tval,
        "data" .= decodeUtf8 (B16.encode td),
        "r" .= tr,
        "s" .= ts,
        "v" .= T.take 2 ((<> T.pack "00") . decodeUtf8 . B16.encode . integer2Bytes $ toInteger $ tv + 0x1b)
      ]
        ++ (("chainId" .=) <$> maybeToList tcid)
        ++ (("metadata" .=) <$> maybeToList md)
  toJSON (ContractCreationTX tnon tgp tgl tval tcode tcid tr ts tv md) = 
    object $
      [ "nonce" .= tnon,
        "gasPrice" .= tgp,
        "gasLimit" .= tgl,
        "value" .= show tval,
        "init" .= decodeUtf8 (B16.encode tcode),
        "r" .= tr,
        "s" .= ts,
        "v" .= T.take 2 ((<> T.pack "00") . decodeUtf8 . B16.encode . integer2Bytes $ toInteger tv + 0x1b)
      ]
        ++ (("chainId" .=) <$> maybeToList tcid)
        ++ (("metadata" .=) <$> maybeToList md)

instance FromJSON Transaction where
  parseJSON (Data.Aeson.Object t) = do
    tto <- t .:? "to"
    tnon <- t .:? "nonce" .!= 0
    tgp <- t .:? "gasPrice" .!= 0
    tgl <- t .:? "gasLimit" .!= 0
    tval <- read <$> (t .:? "value" .!= "0")
    tcid <- t .:? "chainId"
    tr <- t .: "r"
    ts <- t .: "s"
    tv <- fst . head . readHex <$> (t .:? "v" .!= "0")
    md <- t .:? "metadata"

    case tto of
      Nothing -> do
        ti' <- t .: "init"
        case B16.decode $ encodeUtf8 ti' of
          Right ti -> pure $ ContractCreationTX tnon tgp tgl tval ti tcid tr ts tv md
          Left e -> fail e
      (Just to') -> do
        td' <- t .: "data"
        case B16.decode $ encodeUtf8 td' of
          Right td -> pure $ MessageTX tnon tgp tgl to' tval td tcid tr ts tv md
          Left e -> fail e
  parseJSON _ = error "bad param when calling parseJSON for Transaction"

instance RLPSerializable Transaction where
  rlpDecode (RLPArray (n : gp : gl : toAddr : val : i : vVal : rVal : sVal : xs)) =
    let (cid, md) = case xs of
          [] -> (Nothing, Nothing)
          [c] -> case c of
            (RLPArray a) -> (Nothing, Just . M.fromList $ map ((decodeUtf8 *** decodeUtf8) . rlpDecode) a)
            cid' -> (Just $ rlpDecode cid', Nothing)
          (c : (RLPArray a) : _) -> (Just $ rlpDecode c, Just . M.fromList $ map ((decodeUtf8 *** decodeUtf8) . rlpDecode) a)
          (_ : o : _) -> error $ "rlpDecode Transaction: Expected metadata to be an RLPArray, but got: " ++ show o
     in case partial of
          p@MessageTX {} ->
            p
              { transactionV = fromInteger $ rlpDecode vVal,
                transactionR = rlpDecode rVal,
                transactionS = rlpDecode sVal,
                transactionChainId = cid,
                transactionMetadata = md
              }
          p@ContractCreationTX {} ->
            p
              { transactionV = fromInteger $ rlpDecode vVal,
                transactionR = rlpDecode rVal,
                transactionS = rlpDecode sVal,
                transactionChainId = cid,
                transactionMetadata = md
              }
    where
      partial = partialRLPDecode $ RLPArray [n, gp, gl, toAddr, val, i]
  rlpDecode x = error ("rlp object has wrong format in call to rlpDecodeq: " ++ show x)

  rlpEncode t = case r of
    RLPArray (n : gp : gl : toAddr : val : i : cid) ->
      let chainId = listToMaybe cid
       in case t of
            MessageTX {..} ->
              RLPArray $
                [ n,
                  gp,
                  gl,
                  toAddr,
                  val,
                  i,
                  rlpEncode $ toInteger transactionV,
                  rlpEncode $ transactionR,
                  rlpEncode $ transactionS
                ]
                  ++ (maybeToList chainId)
                  ++ (maybeToList $ fmap (RLPArray . map (rlpEncode . (encodeUtf8 *** encodeUtf8)) . M.toList) transactionMetadata)
            ContractCreationTX {..} ->
              RLPArray $
                [ n,
                  gp,
                  gl,
                  toAddr,
                  val,
                  i,
                  rlpEncode $ toInteger transactionV,
                  rlpEncode $ transactionR,
                  rlpEncode $ transactionS
                ]
                  ++ (maybeToList chainId)
                  ++ (maybeToList $ fmap (RLPArray . map (rlpEncode . (encodeUtf8 *** encodeUtf8)) . M.toList) transactionMetadata)
    _ -> error $ "rlpEncode Transaction: Expected RLPArray, but got: " ++ show r
    where
      r = partialRLPEncode t

isMessageTX :: Transaction -> Bool
isMessageTX MessageTX {} = True
isMessageTX _ = False

--partialRLP(De|En)code are used for the signing algorithm
partialRLPDecode :: RLPObject -> Transaction
partialRLPDecode (RLPArray [n, gp, gl, RLPString "", val, i]) =
  --Note- Address 0 /= Address 000000....  Only Address 0 yields a ContractCreationTX
  ContractCreationTX
    { transactionNonce = rlpDecode n,
      transactionGasPrice = rlpDecode gp,
      transactionGasLimit = rlpDecode gl,
      transactionValue = rlpDecode val,
      transactionInit = rlpDecode i,
      transactionChainId = error "transactionChainId not initialized in partialRLPDecode",
      transactionR = error "transactionR not initialized in partialRLPDecode",
      transactionS = error "transactionS not initialized in partialRLPDecode",
      transactionV = error "transactionV not initialized in partialRLPDecode",
      transactionMetadata = error "transactionMetadata not initialized in partialRLPDecode"
    }
partialRLPDecode (RLPArray [n, gp, gl, toAddr, val, i]) =
  MessageTX
    { transactionNonce = rlpDecode n,
      transactionGasPrice = rlpDecode gp,
      transactionGasLimit = rlpDecode gl,
      transactionTo = rlpDecode toAddr,
      transactionValue = rlpDecode val,
      transactionData = rlpDecode i,
      transactionChainId = error "transactionChainId not initialized in partialRLPDecode",
      transactionR = error "transactionR not initialized in partialRLPDecode",
      transactionS = error "transactionS not initialized in partialRLPDecode",
      transactionV = error "transactionV not initialized in partialRLPDecode",
      transactionMetadata = error "transactionMetadata not initialized in partialRLPDecode"
    }
partialRLPDecode x = error ("rlp object has wrong format in call to partialRLPDecode: " ++ show x)

partialRLPEncode :: Transaction -> RLPObject
partialRLPEncode MessageTX {transactionNonce = n, transactionGasPrice = gp, transactionGasLimit = gl, transactionTo = to', transactionValue = v, transactionData = d, transactionChainId = cid} =
  RLPArray $
    [ rlpEncode n,
      rlpEncode gp,
      rlpEncode gl,
      rlpEncode to',
      rlpEncode v,
      rlpEncode d
    ]
      ++ (maybeToList $ fmap rlpEncode cid)
partialRLPEncode ContractCreationTX {transactionNonce = n, transactionGasPrice = gp, transactionGasLimit = gl, transactionValue = v, transactionInit = init', transactionChainId = cid} =
  RLPArray $
    [ rlpEncode n,
      rlpEncode gp,
      rlpEncode gl,
      rlpEncode (0 :: Integer),
      rlpEncode v,
      rlpEncode init'
    ]
      ++ (maybeToList $ fmap rlpEncode cid)
