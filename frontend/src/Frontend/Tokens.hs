{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Tokens where

import           Blockchain.Data.Keccak256
import           Blockchain.Data.Keys
import           Common.Utils
import           Control.Applicative    (liftA2)
import           Control.Lens           ((^.))
import           Control.Monad          ((<=<), join, void)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Bool
import           Data.Functor           ((<&>))
import           Data.List              (find)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe, listToMaybe)
import qualified Data.Text              as T
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8')
import           Frontend.Client        (urlGET)
import           Frontend.ItemList
import           Frontend.Transactions
import           Frontend.Utils
import           JSDOM.Generated.Response (redirect)
import           Language.Javascript.JSaddle
import           Mercata.Data.Item
import           Mercata.Data.Trade
import           Network.HTTP.Types     (urlEncode)
import           Obelisk.Configs
import           Obelisk.Frontend.Storage
import           Reflex.Dom.Core
import           Text.Printf
import           Text.Read              (readMaybe)
import Debug.Trace

newtype Cert = Cert {
  userAddress :: Text
} deriving stock (Eq, Show)

instance FromJSON Cert where
  parseJSON = withObject "Cert" $ \o ->
    Cert <$> (fromMaybe "" <$> (o .:? "userAddress"))

data TokenBalance = TokenBalance {
  balance :: Double
} deriving stock (Eq, Show)

instance FromJSON TokenBalance where
  parseJSON = withObject "TokenBalance" $ \o -> do
    b <- T.pack <$> (o .: "value")
    a' <- o .: "address"
    flip (withObject "TokenBalance.address") a' $ \a -> do
      (d :: Integer) <- a .: "decimals"
      (b :: Maybe Double) <- readMaybe <$> (o .: "value")
      let d' = case b of
            Nothing -> 0.0
            Just b' -> b' / (10 ^ d)
      pure $ TokenBalance d'

tokenWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , Prerender t m
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , HasConfigs m
     )
  => (Text, PrivateKey) -> m (Event t ())
tokenWidget creds = mdo
  let closedWidget = divClass "token-widget" $ do
        divClass "token-balance" $ tokenBalance creds reload
        divClass "dm-serif-display-regular" $ divClass "buy-tokens" $ button "Buy LAM"
      openWidget = do
        divClass "token-widget" $ 
          divClass "token-balance" $ tokenBalance creds reload
        buyTokensWidget creds
  eReload <- toggleWidget ((False <$) <$> closedWidget) openWidget
  let reload = fmapMaybe id $ either (const Nothing) (bool Nothing (Just ())) <$> eReload
  pure $ void reload

buyTokensWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , Prerender t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , HasConfigs m
     )
  => (Text, PrivateKey) -> m (Event t Bool)
buyTokensWidget creds = divClass "trade-widget" $ do
  pBE <- getPostBuild
  let tokenUrl = T.append assetUrl $ "&address=eq." <> tshow tokenPsAddr
  (mTokenItem :: Event t (Maybe Item)) <- fmap (listToMaybe =<<) <$> urlGET (tokenUrl <$ pBE)
  let tokenItem = fmapMaybe id mTokenItem
  itemDyn <- holdDyn emptyItem tokenItem
  let psAddrs = maybe [] (map paymentAddress) . (pure . paymentProviders <=< sale <=< listToMaybe) . M.elems . utxos <$> tokenItem
      getPSUrl = (<> ")") . T.append "https://lambdachain.xyz/cirrus/search/BlockApps-Mercata-PaymentService?address=in.(" . T.intercalate "," . map tshow <$> psAddrs
  (pss :: Event t [PaymentService]) <- fmap (fromMaybe []) <$> urlGET getPSUrl
  let mStripePS = find ((== "Stripe") . paymentServiceName) <$> pss
  stripeDyn <- holdDyn Nothing mStripePS

  (qpDyn, cancelled) <- divClass "trade-params-widget-container" $ do
    qpDyn' <- tradeParamsWidget Nothing itemDyn (constDyn Market)
    (qpDyn',) <$> buttonClass "cancel" "X"
  let qpiDyn = zipDyn qpDyn itemDyn
  mdo
    clickedDyn <- holdDyn False $ leftmost [True <$ started transaction, False <$ finished transaction]
    clickEv <- dynButtonClass "buy" clickedDyn $ bool "Buy LAM" "Buying LAM..." <$> clickedDyn
    let qpEv = fmapMaybe id $ tag (current qpDyn) clickEv

    let tradeDyn = (\((mqp,i), ms) -> (\(qp,stripe) -> Trade Create Market Buy stripe i qp) <$> liftA2 (,) mqp ms) <$> zipDyn qpiDyn (fmap paymentAddress <$> stripeDyn)
    let tradeEv' = fmapMaybe id $ tag (current tradeDyn) clickEv
    transaction <- postTransaction creds tradeEv'
    let responseEv = finished transaction
    let (mOrderHash :: Event t (Maybe Keccak256)) = readMaybe . T.unpack . T.take 64 . T.drop 2 <$> responseEv
    mOrderHashDyn <- holdDyn Nothing mOrderHash
    asdf <- holdDyn (Nothing,Nothing) (attach (current stripeDyn) mOrderHash) 
    let cfg = "common/route"
        path = "config/" <> cfg
    meCfg <- getConfig cfg >>= \case
      Nothing -> pure . Left $ "No config file found in " <> path
      Just bytes -> case decodeUtf8' $ urlEncode True (bytes <> "/completeOrder") of
        Left ue -> pure . Left $ "Couldn't decode " <> path <> " : " <> T.pack (show ue)
        Right s -> pure $ Right s
    redirectDyn <- widgetHold (pure ()) $ attach (current stripeDyn) mOrderHash <&> \(mStripe, mOrderHash) -> case liftA2 (,) mStripe mOrderHash of
      Nothing -> pure ()
      Just (stripe, orderHash) -> do
        prerender_ blank $ case meCfg of 
          Left t -> text t
          Right redirectUrl -> do
            let url = paymentServiceURL stripe <> checkoutRoute stripe <> "?orderHash=" <> tshow orderHash <> "&redirectUrl=" <> redirectUrl
            -- resp <- redirect (paymentServiceURL stripe <> checkoutRoute stripe <> "?orderHash=" <> tshow orderHash <> "&redirectUrl=http%3A%2F%2Flocalhost%3A8000") (Just 0)
            _ <- liftJSM $ do
              eval . T.unpack $ "window.location.replace(\"" <> url <> "\");"
              -- l <- jsg1 ("window.location" :: String) ("location" :: String)
              -- _ <- l ^. js1 ("replace" :: String) (T.unpack url)
              -- s <- jsg ("console" :: String) 
              -- _ <- s ^. js1 ("log" :: String) resp
              pure ()
            pure ()

    pure $ leftmost [True <$ updated redirectDyn, False <$ cancelled]

tokenBalance
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , Prerender t m
     )
  => (Text, PrivateKey) -> Event t () -> m ()
tokenBalance creds reload = elAttr "div" ("style" =: "font-size: 18px;") $ do
  pBE <- getPostBuild
  -- let commonNameQuery = ("&commonName=eq." <>) <$> leftmost [fst <$> updated creds, tag (fst <$> current creds) pBE]
  let commonNameKeyQuery = ("&key=eq." <> fst creds) <$ leftmost [reload, pBE]
  -- let certUrl = ("https://lambdachain.xyz/cirrus/search/Certificate?limit=1" <>) <$> commonNameQuery
  -- (mCertsEv :: Event t (Maybe [Cert])) <- urlGET certUrl
  -- let certEv = fmapMaybe (maybe Nothing listToMaybe) mCertsEv
  -- let addressQuery = ("&key=eq." <>) . userAddress <$> certEv
  let tokenUrl = T.append ("https://lambdachain.xyz/cirrus/search/gotan-TokenPaymentService-balances?address=eq." <> tshow tokenPsAddr <> "&select=value,address(*)&key=eq." <> fst creds) <$> commonNameKeyQuery
  (mTokenBalancesEv :: Event t (Maybe [TokenBalance])) <- urlGET tokenUrl
  text "LAM Balance: λ"
  _ <- widgetHold blank $ ffor mTokenBalancesEv $ \case
    Just (tokenBal:_) -> do
      let b :: Double
          b = balance tokenBal
          t = T.pack (printf "%0.2f" b)
      text t
    _ -> text "0.00"
  pure ()