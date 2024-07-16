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

data TradeAction = Buy | Sell
  deriving stock (Eq, Show)

data TradeType = Market | Limit
  deriving stock (Eq, Show)

data QuantityAndPrice = QuantityAndPrice {
  _qp_quantity :: Double,
  _qp_price    :: Maybe Double
} deriving stock (Eq, Show)

data Trade = Trade {
  _trade_action :: TradeAction,
  _trade_type   :: TradeType,
  _trade_qp     :: QuantityAndPrice
} deriving stock (Eq, Show)