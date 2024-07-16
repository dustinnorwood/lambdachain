{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Frontend.Shop where

import Reflex.Dom.Core

import           Blockchain.Data.Address
import           Blockchain.Data.AddressState
import           Blockchain.Data.ExtendedWord
import           Blockchain.Data.Keccak256
import           Blockchain.Data.Keys
import           Blockchain.Data.RLP
import           Blockchain.Data.Transaction
import           Common.Route
import           Control.Applicative    (liftA2)
import           Control.Monad          (join)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class
import qualified Data.ByteString        as BS
import qualified Data.Map.Strict        as M
import           Data.Maybe             (listToMaybe)
import qualified Data.Text              as T
import           Data.Text              (Text)
import           Data.Traversable       (for)
import           Frontend.Client        (urlGET, urlPOST)
import           Frontend.ItemList
import           Language.Javascript.JSaddle
import           Mercata.Data.Item
import           Mercata.Data.Trade
import           Obelisk.Route.Frontend (R, SetRoute)

shop
  :: forall t m
  . ( PostBuild t m
     , PerformEvent t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , Prerender t m
     , SetRoute t (R FrontendRoute) m
     )
  => Dynamic t (Maybe (Text, PrivateKey)) -> m (Event t ())
shop mCreds = divClass "container" $ mdo
  pbE <- getPostBuild
  let getShopE = leftmost [() <$ debouncedTxE, pbE, () <$ updated mCreds]
  let stratPsAddr = "7b2575a68a7eee430632089ff06f2ed7cdadb319"
  let itemsForSaleUrl = "https://lambdachain.xyz/cirrus/search/BlockApps-Mercata-Sale-paymentProviders?value=eq." <> stratPsAddr <> "&select=BlockApps-Mercata-Sale(*,BlockApps-Mercata-Asset!BlockApps-Mercata-Sale_BlockApps-Mercata-Asset_fk(*,BlockApps-Mercata-Asset-images(*)))" <$ getShopE
  (mItemsEv :: Event t (Maybe [Item])) <- fmap (fmap $ fmap unItemForSale) <$> urlGET itemsForSaleUrl
  let z = pure $ constDyn []
  mClickedItemDyn <- holdDyn Nothing $ leftmost [Just . fst . fst <$> itemEv, Nothing <$ debouncedTxE]
  itemListDynDyn <- widgetHold z $ maybe z (itemList Buy mClickedItemDyn . Prelude.filter (maybe False isOpen . sale)) <$> mItemsEv
  let itemEv = switchDyn $ leftmost <$> join itemListDynDyn
      credsAndItemEv = attach (current mCreds) itemEv
  credsAndItemDyn <- holdDyn Nothing $ Just <$> credsAndItemEv
  let getAddress pkE = fmap join . prerender (pure $ constDyn Nothing) $ do
        let getAddr = fmap publicKeyToAddress . liftJSM . toPublicKey
        widgetHold (pure Nothing) $ traverse getAddr <$> pkE
  addrDyn <- getAddress $ join . fmap (fmap snd . fst) <$> updated credsAndItemDyn
  let acctStateUrl = ("https://lambdachain.xyz/strato-api/eth/v1.2/account?address=" <>) . T.pack . show <$> fmapMaybe id (updated addrDyn)
  (mAcctStateEv :: Event t (Maybe AddressState)) <- fmap (join . fmap listToMaybe) <$> urlGET acctStateUrl
  mSignedTxDyn <- fmap join . prerender (pure $ constDyn Nothing) $ do
    let getSignedTx ((mpk, ((_,item),trade)), acctState) = for mpk $ \(_,pk) -> do
          let unsignedTx = MessageTX
                 { transactionNonce = nonce acctState,
                   transactionGasPrice = 1,
                   transactionGasLimit = 100000000,
                   transactionTo = read $ T.unpack stratPsAddr,
                   transactionValue = 0,
                   transactionData = BS.empty,
                   transactionChainId = Nothing,
                   transactionR = Word256 0,
                   transactionS = Word256 0,
                   transactionV = 0,
                   transactionMetadata = Just $ M.fromList
                     [ ("VM", "SolidVM")
                     , ("funcName", "createOrder")
                     , ("args",
                        T.concat
                          [ "("
                          , T.pack (show $ nonce acctState + 123456)
                          , ",[0x"
                          , T.pack (show $ maybe (Address $ Word160 0) saleAddress $ sale item)
                          , "],["
                          , T.pack (show $ round $ _qp_quantity $ _trade_qp trade)
                          , "],123456789,\"\")"
                          ]
                       )
                     ]
                 }
          sig <- liftJSM $ sign pk . hashMsg . rlpSerialize $ partialRLPEncode unsignedTx
          let signedTx = unsignedTx
                { transactionR = r sig
                , transactionS = s sig
                , transactionV = v sig
                }
          pure signedTx
    widgetHold (pure Nothing) $ getSignedTx <$> fmapMaybe id (uncurry (liftA2 (,)) <$> attach (current credsAndItemDyn) mAcctStateEv)
  (txE :: Event t (Maybe Keccak256)) <- urlPOST $ (,) "https://lambdachain.xyz/strato-api/eth/v1.2/transaction" <$> fmapMaybe id (updated mSignedTxDyn)
  -- let resolveTx debounceTime hashE = do
  --       (txResultE :: Event (Maybe TransactionResult)) <- fmap (join . fmap listToMaybe) . urlGET $ ("https://lambdachain.xyz/strato-api/eth/v1.2/transactionResult/" <>) . show <$> hashE
  --       let handleResult = \case
  --             Just (TransactionResult True) -> pure $ Just ()
  --             Just _ -> pure Nothing
  --             Nothing -> resolveTx (2*debounceTime) txHash
  --           widgetHold (pure Nothing) $ debounce debounceTime pkE
  --       debouncedTxE <- debounce debounceTime $ fmapMaybe id txE
        
  debouncedTxE <- debounce 2.0 $ fmapMaybe id txE
  el "br" blank
  el "br" blank
  pure $ () <$ debouncedTxE