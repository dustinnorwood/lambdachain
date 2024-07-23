{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
import           Common.Utils
import           Control.Applicative    (liftA2)
import           Control.Monad          (join, void)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class
import           Data.List              (sortOn)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromJust, fromMaybe, listToMaybe)
import qualified Data.Text              as T
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           Frontend.Client        (urlGET, urlPOST)
import           Frontend.ItemList
import           Frontend.Transactions
import           Frontend.Utils
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
  => (Text, PrivateKey) -> m (Event t ())
shop creds = divClass "container" $ mdo
  pBE <- getPostBuild
  searchTerm <- searchWidget
  let getShopE = leftmost [tag (current searchTerm) reload, tag (current searchTerm) pBE, updated searchTerm]
  (mItemsEv :: Event t (Maybe [Item])) <- urlGET $ T.append assetUrl . (\t -> if T.null t then "" else "&name=like.*" <> t <> "*") <$> getShopE
  let z = pure never
  let itemListWidget = itemList Buy (fst creds) $ postTransaction creds
  reload <- fmap switchDyn . widgetHold z $ maybe z (itemListWidget . aggregateItems) <$> mItemsEv
  el "br" blank
  el "br" blank
  pure $ () <$ reload

searchWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     )
  => m (Dynamic t Text)
searchWidget = divClass "searchbar" $ do
  tSearch <- inputElement $ def
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
      ("placeholder" =: "Search" <> "type" =: "text")
  pure $ value tSearch