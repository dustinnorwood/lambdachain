{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Mercata.Data.Trade where

import           Blockchain.Data.Address
import           Data.Aeson
import           Data.List               (find)
import           Data.Maybe              (fromMaybe, listToMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Mercata.Data.Item
import           Reflex

data TradeOperation = Create | Update | Delete
  deriving stock (Eq, Show)

data TradeAction = Buy | Sell
  deriving stock (Eq, Show)

tradeName :: TradeAction -> Text
tradeName Buy  = "Bid"
tradeName Sell = "Ask"

data TradeType = Market | Limit
  deriving stock (Eq, Show)

data QuantityAndPrice = QuantityAndPrice {
  _qp_quantity :: Integer,
  _qp_price    :: Maybe Double
} deriving stock (Eq, Show)

data Trade = Trade {
  _trade_op     :: TradeOperation,
  _trade_type   :: TradeType,
  _trade_action :: TradeAction,
  _trade_ps     :: Address,
  _trade_item   :: Item,
  _trade_qp     :: QuantityAndPrice
} deriving stock (Eq, Show)

data Async t a = Async {
  started  :: Event t (),
  finished :: Event t a
}