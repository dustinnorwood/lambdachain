{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Frontend.HomePage where

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
import qualified Data.ByteString.Base16 as B16
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe, listToMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.Traversable       (for)
import           Frontend.Client        (urlGET, urlPOST)
import           Frontend.ItemList
import           Language.Javascript.JSaddle
import           Mercata.Data.Item
import           Mercata.Data.Trade
import           Obelisk.Frontend.Storage
import           Obelisk.Route.Frontend (R, SetRoute)
import           Reflex.Dom.Core
import           Text.Printf

homePage
  :: forall t m
  . ( PostBuild t m
     , PerformEvent t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , Prerender t m
     , MonadIO (Performable m)
     , SetRoute t (R FrontendRoute) m
     , TriggerEvent t m
     )
  => Dynamic t (Maybe (Text, PrivateKey)) -> m (Event t ())
homePage mCreds = divClass "container" $ mdo
  pBE <- getPostBuild
  username2 <- fmap (fmap snd) . getStorageItem $ "mercata_username" <$ pBE
  let ownerQuery = ("&ownerCommonName=eq." <>) <$> fmapMaybe id (leftmost [fmap fst <$> updated mCreds, username2])
  let stratPsAddr = "7b2575a68a7eee430632089ff06f2ed7cdadb319"
  let assetUrl = ("https://lambdachain.xyz/cirrus/search/BlockApps-Mercata-Asset?limit=100&select=*,BlockApps-Mercata-Asset-images(*)" <>) <$> ownerQuery
  let z = pure $ constDyn []
  (mItemsEv :: Event t (Maybe [Item])) <- urlGET assetUrl
  mClickedItemDyn <- holdDyn Nothing $ leftmost [Just . fst . fst <$> itemEv, Nothing <$ debouncedTxE]
  mItemListDynDyn <- widgetHold z $ ffor mItemsEv $ \case
    Just items@(_:_) -> itemList Sell mClickedItemDyn items
    _ -> do
      text "You don't own any items 😢"
      z
  let itemEv = switchDyn $ leftmost <$> join mItemListDynDyn
      credsAndItemEv = attach (current mCreds) itemEv
  credsAndItemDyn <- holdDyn Nothing $ Just <$> credsAndItemEv
  let getAddress pkE = fmap join . prerender (pure $ constDyn Nothing) $ do
        let getAddr = fmap publicKeyToAddress . liftJSM . toPublicKey
        widgetHold (pure Nothing) $ traverse getAddr <$> pkE
  addrDyn <- getAddress $ join . fmap (fmap snd . fst) <$> updated credsAndItemDyn
  let acctStateUrl = ("https://lambdachain.xyz/strato-api/eth/v1.2/account?address=" <>) . T.pack . show <$> fmapMaybe id (updated addrDyn)
  (mAcctStateEv :: Event t (Maybe AddressState)) <- fmap (join . fmap listToMaybe) <$> urlGET acctStateUrl
  mased <- holdDyn Nothing mAcctStateEv
  dynText $ T.pack . show <$> mased
  mSignedTxDyn <- fmap join . prerender (pure $ constDyn Nothing) $ do
    let getSignedTx ((mpk, ((_,item),trade)), acctState) = for mpk $ \(_,pk) -> do
          let src = "pragma es6;\npragma strict;\nimport <c58b0619586f01668d21dc3508e3e4203a306e8d>;\ncontract SimpleSale is Sale {\nconstructor(address _assetToBeSold,decimal _price,uint _quantity,address[] _paymentProviders) Sale(_assetToBeSold, _price, _quantity, _paymentProviders) {\n}\n}"
          let unsignedTx = ContractCreationTX
                 { transactionNonce = nonce acctState,
                   transactionGasPrice = 1,
                   transactionGasLimit = 100000000,
                   transactionValue = 0,
                   transactionInit = B16.encode $ encodeUtf8 src,
                   transactionChainId = Nothing,
                   transactionR = Word256 0,
                   transactionS = Word256 0,
                   transactionV = 0,
                   transactionMetadata = Just $ M.fromList
                     [ ("VM", "SolidVM")
                     , ("name", "SimpleSale")
                     , ("src", src)
                     , ("args",
                        T.concat
                          [ "(0x"
                          , T.pack (show $ address item)
                          , ","
                          , T.pack (printf "%0.2f" . fromMaybe 0.0 . _qp_price $ _trade_qp trade)
                          , ","
                          , T.pack (show . round . _qp_quantity $ _trade_qp trade)
                          , ",0x"
                          , stratPsAddr
                          , ")"
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
  debouncedTxE <- debounce 2.0 $ fmapMaybe id txE
  el "br" blank
  el "br" blank
  pure $ () <$ debouncedTxE