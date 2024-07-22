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

module Frontend.HomePage where

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
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.List              (sortOn)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromJust, fromMaybe, listToMaybe)
import           Data.Ord               (Down(..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.Traversable       (for)
import           Frontend.Client        (urlGET, urlPOST)
import           Frontend.ItemList
import           Frontend.Transactions
import           Frontend.Tokens
import           Frontend.Utils
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
  _ <- tokenWidget mCreds
  let ownerQuery = ("&ownerCommonName=eq." <>) <$> fmapMaybe id (leftmost [fmap fst <$> updated mCreds, username2])
  (mItemsEv :: Event t (Maybe [Item])) <- urlGET $ T.append assetUrl <$> ownerQuery
  let z = pure never
  el "h2" $ text "My Items"
  el "br" blank
  let itemListWidget = itemList Sell (maybe "" fst <$> mCreds) $ postTransaction mCreds
  reload <- fmap switchDyn . widgetHold z $ ffor mItemsEv $ \case
    Just items@(_:_) -> itemListWidget $ aggregateItems items
    _ -> do
      text "You don't own any items 😢"
      z
  el "br" blank
  el "br" blank
  pure $ () <$ reload