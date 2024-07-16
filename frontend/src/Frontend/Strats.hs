{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Strats where

import           Blockchain.Data.Keys
import           Data.Aeson
import           Data.Maybe             (fromMaybe, listToMaybe)
import qualified Data.Text              as T
import           Data.Text              (Text)
import           Frontend.Client        (urlGET)
import           Obelisk.Frontend.Storage
import           Reflex.Dom.Core
import           Text.Printf

newtype Cert = Cert {
  userAddress :: Text
} deriving stock (Eq, Show)

instance FromJSON Cert where
  parseJSON = withObject "Cert" $ \o ->
    Cert <$> (fromMaybe "" <$> (o .:? "userAddress"))

newtype StratBalance = StratBalance {
  balance :: Text
} deriving stock (Eq, Show)

instance FromJSON StratBalance where
  parseJSON = withObject "StratBalance" $ \o ->
    StratBalance <$> (fromMaybe "" <$> (o .:? "value"))

stratBalance
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , Prerender t m
     )
  => Dynamic t (Maybe (Text, PrivateKey)) -> m ()
stratBalance mCreds = do
  pBE <- getPostBuild
  username2 <- fmap (fmap snd) . getStorageItem $ "mercata_username" <$ pBE
  let commonNameQuery = ("&commonName=eq." <>) <$> fmapMaybe id (leftmost [fmap fst <$> updated mCreds, username2])
  let certUrl = ("https://lambdachain.xyz/cirrus/search/Certificate?limit=1" <>) <$> commonNameQuery
  (mCertsEv :: Event t (Maybe [Cert])) <- urlGET certUrl
  let certEv = fmapMaybe (maybe Nothing listToMaybe) mCertsEv
  let addressQuery = ("&key=eq." <>) . userAddress <$> certEv
  let stratUrl = ("https://lambdachain.xyz/cirrus/search/TestCompany-ERC20Dapp-balances?address=eq.488cd3909d94606051e0684cf6caa5763fb78613" <>) <$> addressQuery
  (mStratBalancesEv :: Event t (Maybe [StratBalance])) <- urlGET stratUrl
  text "STRAT Balance: "
  _ <- widgetHold blank $ ffor mStratBalancesEv $ \case
    Just (stratBal:_) -> do
      let b :: Integer
          b = read . T.unpack $ balance stratBal
          t = T.pack $ show (b `div` 100) <> "." <> printf "%02d" (b `mod` 100)
      text t
    _ -> text "0.00"
  pure ()