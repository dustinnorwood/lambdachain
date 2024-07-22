{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Mercata.Data.Item where

import           Blockchain.Data.Address
import           Control.Applicative     ((<|>))
import           Data.Aeson
import           Data.List               (find)
import qualified Data.List.NonEmpty      as NE
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as M
import           Data.Maybe              (fromMaybe, isJust, listToMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Text.Printf
import Data.Bool (Bool(False))

newtype Image = Image {
  url :: Text
} deriving stock (Eq, Show)

data PaymentService = PaymentService {
  paymentAddress :: Address,
  paymentServiceName :: Text,
  paymentServiceURL  :: Text,
  checkoutRoute      :: Text,
  isActive :: Bool
} deriving stock (Eq, Show)

data Sale = Sale {
  saleAddress :: Address,
  salePrice :: Double,
  saleQuantity :: Integer,
  paymentProviders :: [PaymentService],
  isOpenSale :: Bool
} deriving stock (Eq, Show)

data UTXO = UTXO {
  address         :: Address,
  ownerCommonName :: Text,
  itemNumber      :: Integer,
  itemQuantity    :: Integer,
  sale            :: Maybe Sale
} deriving stock (Eq, Show)

data Bid = Bid {
  bidAddress :: Address,
  bidPrice :: Double,
  bidQuantity :: Integer,
  purchaserCommonName :: Text,
  isOpenBid :: Bool
} deriving stock (Eq, Show)

data Item = Item {
  root         :: Address,
  name         :: Text,
  description  :: Text,
  images       :: [Image],
  utxos        :: Map Address UTXO,
  bids         :: Map Address Bid
} deriving stock (Eq, Show)

instance FromJSON Image where
  parseJSON = withObject "Image" $ \o ->
    Image <$> (fromMaybe "" <$> (o .:? "value"))

instance FromJSON PaymentService where
  parseJSON = withObject "PaymentService" $ \o -> do
    mData <- (o .:? "data")
    (su, cr) <- case mData of
      Nothing -> pure ("", "")
      Just data' -> flip (withObject "PaymentService.data") data' $ \d -> do
        (,) <$> (fromMaybe "" <$> (d .:? "serviceURL"))
            <*> (fromMaybe "" <$> (d .:? "checkoutRoute"))
    PaymentService <$> (do 
                          v <- o .:? "value"
                          case v of
                            Just a -> pure a
                            Nothing -> fromMaybe zeroAddress <$> (o .:? "address")
                       )
                   <*> (fromMaybe "" <$> (o .:? "serviceName"))
                   <*> pure su
                   <*> pure cr
                   <*> pure True -- (o .: "isActive")

instance FromJSON Sale where
  parseJSON = withObject "Sale" $ \o ->
    Sale <$> (o .: "address")
         <*> (fromMaybe 0.0 <$> (o .:? "price"))
         <*> (fromMaybe 0 <$> (o .:? "quantity"))
         <*> (o .: "BlockApps-Mercata-Sale-paymentProviders")
         <*> (fromMaybe False <$> (o .:? "isOpen"))

instance FromJSON Bid where
  parseJSON = withObject "Bid" $ \o ->
    Bid <$> (o .: "address")
        <*> (fromMaybe 0.0 <$> (o .:? "price"))
        <*> (fromMaybe 0 <$> (o .:? "quantity"))
        <*> (fromMaybe "" <$> (o .:? "purchaserCommonName"))
        <*> (fromMaybe False <$> (o .:? "isOpen"))

instance FromJSON Item where
  parseJSON = withObject "Asset" $ \a -> do
    utxo <- UTXO <$> (a .: "address")
                 <*> (fromMaybe "" <$> (a .:? "ownerCommonName"))
                 <*> (fromMaybe 1 <$> (a .:? "itemNumber"))
                 <*> (fromMaybe 0 <$> (a .:? "quantity"))
                 <*> (find isOpenSale . fromMaybe [] <$> (a .:? "BlockApps-Mercata-Sale"))
    Item <$> (a .: "root")
         <*> (fromMaybe "" <$> (a .:? "name"))
         <*> (fromMaybe "" <$> (a .:? "description"))
         <*> (fmap normalize . fromMaybe [] <$> (a .:? "BlockApps-Mercata-Asset-images"))
         <*> (pure $ M.singleton (address utxo) utxo)
         <*> (M.fromList . map (\b -> (bidAddress b, b)) . fromMaybe [] <$> (a .:? "BlockApps-Mercata-Bid"))
    where normalize (Image i) | T.take 4 i == "http" = Image i
                              | otherwise            = Image ("https://fileserver.mercata-testnet2.blockapps.net/highway/" <> i)

emptyItem :: Item
emptyItem = Item zeroAddress "" "" [] M.empty M.empty

adjustItemMap :: Item -> Map Address Item -> Map Address Item
adjustItemMap i m = case M.lookup (root i) m of
  Nothing -> M.insert (root i) i m
  Just i' -> M.insert (root i) i'{utxos = utxos i' <> utxos i, bids = bids i' <> bids i} m

aggregateItems :: [Item] -> [Item]
aggregateItems = M.elems . Prelude.foldr adjustItemMap M.empty

quantity :: Item -> Integer
quantity item = case M.lookup (root item) (utxos item) of
  Just u -> itemNumber u + itemQuantity u
  Nothing -> sum $ itemQuantity <$> M.elems (utxos item)

ownedBy :: Text -> Item -> Item
ownedBy user item = item{utxos = M.filter (\u -> itemQuantity u > 0 && ownerCommonName u == user) $ utxos item}

bidded :: Item -> Item
bidded item = item{bids = M.filter isOpenBid $ bids item}

biddedBy :: Text -> Item -> Item
biddedBy user item = item{bids = M.filter (\b -> isOpenBid b && (purchaserCommonName b == user)) $ bids item}

quantityForSale :: Item -> Integer
quantityForSale item = sum $ maybe 0 saleQuantity . sale <$> M.elems (utxos item)

forSale :: Item -> Item
forSale item = item{utxos = M.filter (maybe False isOpenSale . sale) $ utxos item}

forSaleBy :: Text -> Item -> Item
forSaleBy me item = item{utxos = M.filter f $ utxos item}
  where f u = case sale u of
                Nothing -> False
                Just s -> ownerCommonName u == me

forSaleBySomeoneElseWithPS :: Text -> Address -> Item -> Item
forSaleBySomeoneElseWithPS me ps item = item{utxos = M.filter f $ utxos item}
  where f u = case sale u of
                Nothing -> False
                Just s -> ownerCommonName u /= me
                       && ps `elem` (paymentAddress <$> paymentProviders s)

salePriceRange :: Item -> Maybe (Either Double (Double, Double))
salePriceRange item = case M.elems . utxos $ forSale item of
  [] -> Nothing
  xs -> Just $ let min' = minimum $ maybe 0 salePrice . sale <$> xs
                   max' = maximum $ maybe 0 salePrice . sale <$> xs
                in if min' == max'
                     then Left min'
                     else Right (min', max')

bidPriceRange :: Item -> Maybe (Either Double (Double, Double))
bidPriceRange item = case M.elems . bids $ bidded item of
  [] -> Nothing
  xs -> Just $ let min' = minimum $ bidPrice <$> xs
                   max' = maximum $ bidPrice <$> xs
                in if min' == max'
                     then Left min'
                     else Right (min', max')

printPrice :: Double -> Text
printPrice = T.pack . printf "%0.2f"

printPriceUSD :: Double -> Text
printPriceUSD = T.cons '$' . printPrice

-- newtype ItemForSale = ItemForSale { unItemForSale :: Item }
--   deriving stock (Eq, Show)
-- 
-- instance FromJSON ItemForSale where
--   parseJSON = withObject "Item" $ \o -> do
--     s'' <- o .: "BlockApps-Mercata-Sale"
--     flip (withObject "Sale") s'' $ \s' -> do
--       a' <- s' .: "BlockApps-Mercata-Asset"
--       flip (withObject "Asset") a' $ \a -> fmap ItemForSale $ do
--         let utxo = UTXO <$> (a .: "address")
--                         <*> (fromMaybe "" <$> (a .:? "ownerCommonName"))
--                         <*> (fromMaybe 0.0 <$> (a .:? "quantity"))
--                         <*> (find isOpen . fromMaybe [] <$> parseJSON s'')
--         Item <$> (a .: "root")
--              <*> (fromMaybe "" <$> (a .:? "name"))
--              <*> (fromMaybe "" <$> (a .:? "description"))
--              <*> (fromMaybe 0.0 <$> (a .:? "quantity"))
--              <*> (fmap normalize . fromMaybe [] <$> (a .:? "BlockApps-Mercata-Asset-images"))
--              <*> [utxo]
--         Item <$> (a .: "address")
--              <*> (fromMaybe "" <$> (a .:? "name"))
--              <*> (a .: "root")
--              <*> (fromMaybe "" <$> (a .:? "ownerCommonName"))
--              <*> (fromMaybe "" <$> (a .:? "description"))
--              <*> (fromMaybe 0.0 <$> (a .:? "quantity"))
--              <*> (fromMaybe [] <$> (a .:? "BlockApps-Mercata-Asset-images"))
--              <*> (Just <$> parseJSON s'')