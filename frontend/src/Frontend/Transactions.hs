{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Frontend.Transactions where

import           Blockchain.Data.Address
import           Blockchain.Data.AddressState
import           Blockchain.Data.ExtendedWord
import           Blockchain.Data.Keccak256
import           Blockchain.Data.Keys
import           Blockchain.Data.RLP
import           Blockchain.Data.Transaction
import           Blockchain.Data.TransactionResult
import           Common.Route
import           Common.Utils
import           Control.Applicative    (liftA2)
import           Control.Concurrent     (threadDelay)
import           Control.Monad          (join, void)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Functor           ((<&>))
import           Data.List              (sortOn)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (catMaybes, fromJust, fromMaybe, listToMaybe)
import           Data.Ord               (Down(..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.Traversable       (for)
import           Frontend.Client        (urlGET, urlPOST)
import           Frontend.ItemList
import           Frontend.Utils
import           Language.Javascript.JSaddle
import           Mercata.Data.Item
import           Mercata.Data.Trade
import           Obelisk.Frontend.Storage
import           Obelisk.Route.Frontend (R, SetRoute)
import           Reflex.Dom.Core        hiding (Delete)
import           Text.Printf

postTransaction
  :: ( PostBuild t m
     , PerformEvent t m
     , MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , Prerender t m
     , Adjustable t m
     )
  => (Text, PrivateKey) -> Event t Trade -> m (Async t Text)
postTransaction creds@(username, pk) tradeEv = do
  tradeDyn <- holdDyn Nothing $ Just <$> tradeEv
  let getAddress pkE = fmap join . prerender (pure $ constDyn Nothing) $ do
        let getAddr = fmap publicKeyToAddress . liftJSM . toPublicKey
        widgetHold (pure Nothing) (fmap Just . getAddr <$> pkE)
  addrDyn <- getAddress $ snd creds <$ tradeEv
  let acctStateUrl = ("https://lambdachain.xyz/strato-api/eth/v1.2/account?address=" <>) . T.pack . show <$> fmapMaybe id (updated addrDyn)
  acctStateEv <- fmap (fromMaybe blankAddressState . (listToMaybe =<<)) <$> urlGET acctStateUrl
  let reorganize stuff = case stuff of
        (Just t, a) -> Just (t,a)
        _ -> Nothing
  let allInfo = fmapMaybe id $ reorganize <$> attach (current tradeDyn) acctStateEv
  mSignedTxDyn <- fmap join . prerender (pure $ constDyn Nothing) $ do
    let getSignedTxs (trade, acctState) = do
          let buildTransactions = case (_trade_op trade, _trade_action trade) of
                (Create, Buy)  -> buildCreateBuyTransactions
                (Create, Sell) -> buildCreateSellTransactions
                (Update, Buy)  -> buildUpdateBuyTransactions
                (Update, Sell) -> buildUpdateSellTransactions
                (Delete, Buy)  -> buildDeleteBuyTransactions
                (Delete, Sell) -> buildDeleteSellTransactions
          Just <$> traverse (signTx pk) (buildTransactions username acctState trade)
    widgetHold (pure Nothing) $ getSignedTxs <$> allInfo
  (txE :: Event t Keccak256) <- fmap (fmap $ fromMaybe emptyHash) . urlPOST $ (,) "https://lambdachain.xyz/strato-api/eth/v1.2/transaction" <$> fmapMaybe (listToMaybe =<<) (updated mSignedTxDyn)
  respDyn <- widgetHold (pure never) $ txE <&> \txHash -> mdo
    pBE <- getPostBuild
    let tryAgain = leftmost [notDone, pBE]
    delayedEv <- performEvent $ liftIO (threadDelay 200000) <$ tryAgain
    (txResultE :: Event t (Maybe [TransactionResult])) <- urlGET $ ("https://lambdachain.xyz/strato-api/eth/v1.2/transactionResult/" <> tshow txHash) <$ delayedEv
    let isDone = txResultE <&> \case
          Just ((TransactionResult True resp):_) -> Just $ Right resp
          Just (_:_) -> Just $ Left ()
          _ -> Nothing
    let notDone = fmapMaybe (maybe (Just ()) (const Nothing)) isDone
        done = fmapMaybe id isDone
    pure done
  pure $ Async (void txE) (either (const "Transaction failed") id <$> switchDyn respDyn)

signTx :: MonadJSM m => PrivateKey -> Transaction -> m Transaction
signTx pk unsignedTx = do
  sig <- liftJSM $ sign pk . hashMsg . rlpSerialize $ partialRLPEncode unsignedTx
  let signedTx = unsignedTx
        { transactionR = r sig
        , transactionS = s sig
        , transactionV = v sig
        }
  pure signedTx

buildCreateBuyTransactions :: Text -> AddressState -> Trade -> [Transaction]
buildCreateBuyTransactions username acctState trade =
  let item = _trade_item trade
      rootText = tshow $ root item
      QuantityAndPrice{..} = _trade_qp trade
      setNonce n t = t{transactionNonce = n}
      zipNonces = zipWith setNonce [(nonce acctState)..]
      unsignedTx = MessageTX
        { transactionNonce = nonce acctState,
          transactionGasPrice = 1,
          transactionGasLimit = 100000000,
          transactionTo = _trade_ps trade,
          transactionValue = 0,
          transactionData = encodeUtf8 "This transaction was made by the Mercata Book app!",
          transactionChainId = Nothing,
          transactionR = Word256 0,
          transactionS = Word256 0,
          transactionV = 0,
          transactionMetadata = Just $ M.fromList [("VM", "SolidVM")]
        }
      unsignedTxs = zipNonces $ repeat unsignedTx
      mkFillAskTx q i tx =
        let md' = M.fromList [
                ("funcName", "createOrder"),
                ("args",
                  T.concat
                    [ "("
                    , tshow $ transactionNonce tx + 423456
                    , ",[0x"
                    , tshow . maybe (Address $ Word160 0) saleAddress $ sale i
                    , "],["
                    , tshow q
                    , "],123456789,\"\")"
                    ]
                )
              ]
         in tx{transactionMetadata = liftA2 (<>) (transactionMetadata tx) (Just md')}
      mkPlaceBidTx q p tx =
        let md' = M.fromList [
                ("funcName", "openBid"),
                ("args",
                  T.concat
                    [ "(0x"
                    , rootText
                    , ","
                    , printPrice p
                    , ","
                    , tshow q
                    , ")"
                    ]
                )
              ]
         in tx{transactionMetadata = liftA2 (<>) (transactionMetadata tx) (Just md')}
   in case _trade_type trade of
        Market ->
          let item' = _trade_item trade
              openAsks = sortOn (salePrice . fromJust . sale) . M.elems . utxos $
                forSaleBySomeoneElseWithPS username (_trade_ps trade) item'
              takeAsks remQ hp []    = (remQ, hp, [])
              takeAsks remQ _ (x:xs) =
                let q' = maybe 0 saleQuantity $ sale x
                    p' = maybe 0.0 salePrice $ sale x
                 in if q' >= remQ
                      then (0, p', [(remQ, x)])
                      else let (remQ', hp, xs') = takeAsks (remQ - q') p' xs
                            in (remQ', hp, (q', x):xs')
              (remainingQ, highPrice, asks) = takeAsks _qp_quantity 0.0 openAsks
              bidFs = if remainingQ == 0
                        then []
                        else [mkPlaceBidTx remainingQ highPrice]
              askFs = uncurry mkFillAskTx <$> asks
              allFs = askFs ++ bidFs
           in zipWith ($) allFs unsignedTxs
        Limit ->
          let item' = _trade_item trade
              p' = fromMaybe 0.0 _qp_price
              openAsks = sortOn (salePrice . fromJust . sale) . M.elems . utxos $
                forSaleBySomeoneElseWithPS username (_trade_ps trade) item'
              takeAsks remQ []    = (remQ, [])
              takeAsks remQ (x:xs) =
                let q' = maybe 0 saleQuantity $ sale x
                    p'' = maybe 0.0 salePrice $ sale x
                 in if p'' > p'
                      then (remQ, [])
                      else if q' >= remQ
                             then (0, [(remQ, x)])
                             else let (remQ', xs') = takeAsks (remQ - q') xs
                                   in (remQ', (q', x):xs')
              (remainingQ, asks) = takeAsks _qp_quantity openAsks
              bidFs = if remainingQ == 0
                        then []
                        else [mkPlaceBidTx remainingQ p']
              askFs = uncurry mkFillAskTx <$> asks
              allFs = askFs ++ bidFs
           in zipWith ($) allFs unsignedTxs

buildUpdateBuyTransactions :: Text -> AddressState -> Trade -> [Transaction]
buildUpdateBuyTransactions username acctState trade =
  let item = _trade_item trade
      rootText = tshow $ root item
      QuantityAndPrice{..} = _trade_qp trade
      setNonce n t = t{transactionNonce = n}
      zipNonces = zipWith setNonce [(nonce acctState)..]
      unsignedTx = MessageTX
        { transactionNonce = nonce acctState,
          transactionGasPrice = 1,
          transactionGasLimit = 100000000,
          transactionTo = _trade_ps trade,
          transactionValue = 0,
          transactionData = encodeUtf8 "This transaction was made by the Mercata Book app!",
          transactionChainId = Nothing,
          transactionR = Word256 0,
          transactionS = Word256 0,
          transactionV = 0,
          transactionMetadata = Just $ M.fromList [("VM", "SolidVM")]
        }
      unsignedTxs = zipNonces $ repeat unsignedTx
      mkUpdateBidTx q p a tx =
        let md' = M.fromList [
                ("funcName", "update"),
                ("args",
                  T.concat
                    [ "("
                    , tshow q
                    , ","
                    , printPrice p
                    , ",3)" -- 3 is the bitmask used to update both fields
                    ]
                )
              ]
         in tx{transactionTo = a, transactionMetadata = liftA2 (<>) (transactionMetadata tx) (Just md')}
      item' = _trade_item trade
      p' = fromMaybe 0.0 _qp_price
      bids' = M.elems $ bids item'
      allFs = mkUpdateBidTx _qp_quantity p' . bidAddress <$> bids'
   in zipWith ($) allFs unsignedTxs

buildDeleteBuyTransactions :: Text -> AddressState -> Trade -> [Transaction]
buildDeleteBuyTransactions username acctState trade =
  let item = _trade_item trade
      rootText = tshow $ root item
      QuantityAndPrice{..} = _trade_qp trade
      setNonce n t = t{transactionNonce = n}
      zipNonces = zipWith setNonce [(nonce acctState)..]
      unsignedTx = MessageTX
        { transactionNonce = nonce acctState,
          transactionGasPrice = 1,
          transactionGasLimit = 100000000,
          transactionTo = _trade_ps trade,
          transactionValue = 0,
          transactionData = encodeUtf8 "This transaction was made by the Mercata Book app!",
          transactionChainId = Nothing,
          transactionR = Word256 0,
          transactionS = Word256 0,
          transactionV = 0,
          transactionMetadata = Just $ M.fromList [("VM", "SolidVM")]
        }
      unsignedTxs = zipNonces $ repeat unsignedTx
      mkDeleteBidTx a tx =
        let md' = M.fromList [
                ("funcName", "closeBid"),
                ("args", "()")
              ]
         in tx{transactionTo = a, transactionMetadata = liftA2 (<>) (transactionMetadata tx) (Just md')}
      item' = _trade_item trade
      bids' = M.elems $ bids item'
      allFs = mkDeleteBidTx . bidAddress <$> bids'
   in zipWith ($) allFs unsignedTxs

buildCreateSellTransactions :: Text -> AddressState -> Trade -> [Transaction]
buildCreateSellTransactions username acctState trade =
  let src = "pragma es6;\npragma strict;\nimport <c58b0619586f01668d21dc3508e3e4203a306e8d>;\ncontract SimpleSale is Sale {\nconstructor(address _assetToBeSold,decimal _price,uint _quantity,address[] _paymentProviders) Sale(_assetToBeSold, _price, _quantity, _paymentProviders) {\n}\n}"
      item = _trade_item trade
      rootText = tshow $ root item
      QuantityAndPrice{..} = _trade_qp trade
      setNonce n t = t{transactionNonce = n}
      zipNonces = zipWith setNonce [(nonce acctState)..]
      unsignedCreationTx = ContractCreationTX
        { transactionNonce = nonce acctState,
          transactionGasPrice = 1,
          transactionGasLimit = 100000000,
          transactionValue = 0,
          transactionInit = encodeUtf8 src,
          transactionChainId = Nothing,
          transactionR = Word256 0,
          transactionS = Word256 0,
          transactionV = 0,
          transactionMetadata = Just $ M.fromList [("VM", "SolidVM")]
        }
      unsignedCallTx = MessageTX
        { transactionNonce = nonce acctState,
          transactionGasPrice = 1,
          transactionGasLimit = 100000000,
          transactionTo = _trade_ps trade,
          transactionValue = 0,
          transactionData = encodeUtf8 "This transaction was made by the Mercata Book app!",
          transactionChainId = Nothing,
          transactionR = Word256 0,
          transactionS = Word256 0,
          transactionV = 0,
          transactionMetadata = Just $ M.fromList [("VM", "SolidVM")]
        }
      mkCreateAskTx p q i tx =
        let md' = M.fromList
              [ ("name", "SimpleSale")
              , ("src", src)
              , ("args",
                 T.concat
                   [ "(0x"
                   , tshow $ address i
                   , ","
                   , printPrice p
                   , ","
                   , tshow q
                   , ",[0x"
                   , tshow (_trade_ps trade)
                   , "])"
                   ]
                )
              ]
         in tx{transactionMetadata = liftA2 (<>) (transactionMetadata tx) (Just md')}
      mkFillBidTx quss b tx =
        let md' = M.fromList [
                ("funcName", "fillBid"),
                ("args",
                  T.concat
                    [ "("
                    , tshow $ sum $ fst <$> quss
                    , "["
                    , T.concat $ ("0x" <>) . tshow . address . snd <$> quss
                    , "])"
                    ]
                )
              ]
         in tx{transactionTo = bidAddress b, transactionMetadata = liftA2 (<>) (transactionMetadata tx) (Just md')}
   in case _trade_type trade of
        Market ->
          let item' = ownedBy username $ _trade_item trade
              unlistedUtxos = sortOn itemQuantity . M.elems $ utxos item'
              openBids = sortOn (Down . bidPrice) . M.elems . bids $
                bidded item'
              fillBid remQ [] = (remQ, [], [])
              fillBid remQ (u:us) =
                let q' = itemQuantity u
                 in if itemQuantity u >= remQ
                      then (0, [(remQ,u)], us)
                      else let (remQ', filled, us') = fillBid (remQ - q') us
                            in (remQ', (q',u):filled, us')
              takeBids remQ lp []  uus = (remQ, lp, [], uus)
              takeBids remQ _ (b:bs) uus =
                let q' = bidQuantity b
                    p' = bidPrice b
                 in if remQ <= q'
                      then let (unfilledQ, filled, us) = fillBid remQ uus
                            in (unfilledQ, p', [(filled,b)], us)
                      else let (unfilledQ, filled, us) = fillBid q' uus
                            in if unfilledQ > 0
                                 then (remQ + unfilledQ - q', p', [(filled, b)], us)
                                 else let (remQ', lp, filledBids', us') = takeBids (remQ - q') p' bs us
                                       in (remQ', lp, (filled, b):filledBids', us')
              (remainingQ, lowPrice, filledBids, remainingUnlisted) = takeBids _qp_quantity 0.0 openBids unlistedUtxos
              takeUnlisted remQ _ []      = (remQ, [])
              takeUnlisted remQ lp (x:xs) =
                let q' = itemQuantity x
                 in if remQ <= q'
                      then (0, [(remQ, x)])
                      else let (remQ', xs') = takeUnlisted (remQ - q') lp xs
                            in (remQ', (q', x):xs')
              bidFs = uncurry mkFillBidTx <$> filledBids
              askFs = if remainingQ == 0
                        then []
                        else uncurry (mkCreateAskTx lowPrice) <$> snd (takeUnlisted remainingQ lowPrice remainingUnlisted)
              allFs = (($ unsignedCallTx) <$> bidFs) ++ (($ unsignedCreationTx) <$> askFs)
           in zipNonces allFs
        Limit ->
          let item' = ownedBy username $ _trade_item trade
              qp_price' = fromMaybe 0.0 _qp_price
              unlistedUtxos = sortOn itemQuantity . M.elems $ utxos item'
              openBids = sortOn (Down . bidPrice) . M.elems . bids $
                bidded item'
              fillBid remQ [] = (remQ, [], [])
              fillBid remQ (u:us) =
                let q' = itemQuantity u
                 in if itemQuantity u >= remQ
                      then (0, [(remQ,u)], us)
                      else let (remQ', filled, us') = fillBid (remQ - q') us
                            in (remQ', (q',u):filled, us')
              takeBids remQ []  uus = (remQ, [], uus)
              takeBids remQ (b:bs) uus =
                let q' = bidQuantity b
                    p' = bidPrice b
                 in if p' < qp_price'
                      then (remQ, [], uus)
                      else if remQ <= q'
                             then let (unfilledQ, filled, us) = fillBid remQ uus
                                   in (unfilledQ, [(filled,b)], us)
                             else let (unfilledQ, filled, us) = fillBid q' uus
                                   in if unfilledQ > 0
                                        then (remQ + unfilledQ - q', [(filled, b)], us)
                                        else let (remQ', filledBids', us') = takeBids (remQ - q') bs us
                                              in (remQ', (filled, b):filledBids', us')
              (remainingQ, filledBids, remainingUnlisted) = takeBids _qp_quantity openBids unlistedUtxos
              takeUnlisted remQ []      = (remQ, [])
              takeUnlisted remQ (x:xs) =
                let q' = itemQuantity x
                 in if remQ <= q'
                      then (0, [(remQ, x)])
                      else let (remQ', xs') = takeUnlisted (remQ - q') xs
                            in (remQ', (q', x):xs')
              bidFs = uncurry mkFillBidTx <$> filledBids
              askFs = if remainingQ == 0
                        then []
                        else uncurry (mkCreateAskTx qp_price') <$> snd (takeUnlisted remainingQ remainingUnlisted)
              allFs = (($ unsignedCallTx) <$> bidFs) ++ (($ unsignedCreationTx) <$> askFs)
           in zipNonces allFs

buildUpdateSellTransactions :: Text -> AddressState -> Trade -> [Transaction]
buildUpdateSellTransactions username acctState trade =
  let item = _trade_item trade
      rootText = tshow $ root item
      QuantityAndPrice{..} = _trade_qp trade
      setNonce n t = t{transactionNonce = n}
      zipNonces = zipWith setNonce [(nonce acctState)..]
      unsignedTx = MessageTX
        { transactionNonce = nonce acctState,
          transactionGasPrice = 1,
          transactionGasLimit = 100000000,
          transactionTo = _trade_ps trade,
          transactionValue = 0,
          transactionData = encodeUtf8 "This transaction was made by the Mercata Book app!",
          transactionChainId = Nothing,
          transactionR = Word256 0,
          transactionS = Word256 0,
          transactionV = 0,
          transactionMetadata = Just $ M.fromList [("VM", "SolidVM")]
        }
      unsignedTxs = zipNonces $ repeat unsignedTx
      mkUpdateAskTx q p a tx =
        let md' = M.fromList [
                ("funcName", "update"),
                ("args",
                  T.concat
                    [ "("
                    , tshow q
                    , ","
                    , printPrice p
                    , ",[],3)" -- 3 is the bitmask used to update quantity and price
                    ]
                )
              ]
         in tx{transactionTo = a, transactionMetadata = liftA2 (<>) (transactionMetadata tx) (Just md')}
      item' = _trade_item trade
      p' = fromMaybe 0.0 _qp_price
      asks' = catMaybes . map sale . M.elems $ utxos item'
      allFs = mkUpdateAskTx _qp_quantity p' . saleAddress <$> asks'
   in zipWith ($) allFs unsignedTxs

buildDeleteSellTransactions :: Text -> AddressState -> Trade -> [Transaction]
buildDeleteSellTransactions username acctState trade =
  let item = _trade_item trade
      rootText = tshow $ root item
      QuantityAndPrice{..} = _trade_qp trade
      setNonce n t = t{transactionNonce = n}
      zipNonces = zipWith setNonce [(nonce acctState)..]
      unsignedTx = MessageTX
        { transactionNonce = nonce acctState,
          transactionGasPrice = 1,
          transactionGasLimit = 100000000,
          transactionTo = _trade_ps trade,
          transactionValue = 0,
          transactionData = encodeUtf8 "This transaction was made by the Mercata Book app!",
          transactionChainId = Nothing,
          transactionR = Word256 0,
          transactionS = Word256 0,
          transactionV = 0,
          transactionMetadata = Just $ M.fromList [("VM", "SolidVM")]
        }
      unsignedTxs = zipNonces $ repeat unsignedTx
      mkDeleteAskTx a tx =
        let md' = M.fromList [
                ("funcName", "closeSale"),
                ("args", "()")
              ]
         in tx{transactionTo = a, transactionMetadata = liftA2 (<>) (transactionMetadata tx) (Just md')}
      item' = _trade_item trade
      asks' = catMaybes . map sale . M.elems $ utxos item'
      allFs = mkDeleteAskTx . saleAddress <$> asks'
   in zipWith ($) allFs unsignedTxs
