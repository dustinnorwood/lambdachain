{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Mercata.Data.Item where

import           Blockchain.Data.Address
import           Data.Aeson
import           Data.List               (find)
import           Data.Maybe              (fromMaybe, listToMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T

newtype Image = Image {
  url :: Text
} deriving stock (Eq, Show)

data Sale = Sale {
  saleAddress :: Address,
  price :: Double,
  saleQuantity :: Double,
  isOpen :: Bool
} deriving stock (Eq, Show)

data Item = Item {
  address :: Address,
  name :: Text,
  root :: Address,
  ownerCommonName :: Text,
  description :: Text,
  quantity :: Double,
  images :: [Image],
  sale :: Maybe Sale
} deriving stock (Eq, Show)

instance FromJSON Image where
  parseJSON = withObject "Image" $ \o ->
    Image <$> (fromMaybe "" <$> (o .:? "value"))

instance FromJSON Sale where
  parseJSON = withObject "Sale" $ \o ->
    Sale <$> (o .: "address")
         <*> (fromMaybe 0.0 <$> (o .:? "price"))
         <*> (fromMaybe 0.0 <$> (o .:? "quantity"))
         <*> (fromMaybe False <$> (o .:? "isOpen"))

instance FromJSON Item where
  parseJSON = withObject "Asset"$ \a -> do
        Item <$> (a .: "address")
             <*> (fromMaybe "" <$> (a .:? "name"))
             <*> (a .: "root")
             <*> (fromMaybe "" <$> (a .:? "ownerCommonName"))
             <*> (fromMaybe "" <$> (a .:? "description"))
             <*> (fromMaybe 0.0 <$> (a .:? "quantity"))
             <*> (fmap normalize . fromMaybe [] <$> (a .:? "BlockApps-Mercata-Asset-images"))
             <*> (find isOpen . fromMaybe [] <$> (a .:? "BlockApps-Mercata-Sale"))
             -- <*> parseJSON s'
    where normalize (Image i) | T.take 4 i == "http" = Image i
                              | otherwise            = Image ("https://fileserver.mercata-testnet2.blockapps.net/highway/" <> i)

newtype ItemForSale = ItemForSale { unItemForSale :: Item }
  deriving stock (Eq, Show)

instance FromJSON ItemForSale where
  parseJSON = withObject "Item" $ \o -> do
    s'' <- o .: "BlockApps-Mercata-Sale"
    flip (withObject "Sale") s'' $ \s' -> do
      a' <- s' .: "BlockApps-Mercata-Asset"
      flip (withObject "Asset") a' $ \a -> fmap ItemForSale $ do
        Item <$> (a .: "address")
             <*> (fromMaybe "" <$> (a .:? "name"))
             <*> (a .: "root")
             <*> (fromMaybe "" <$> (a .:? "ownerCommonName"))
             <*> (fromMaybe "" <$> (a .:? "description"))
             <*> (fromMaybe 0.0 <$> (a .:? "quantity"))
             <*> (fromMaybe [] <$> (a .:? "BlockApps-Mercata-Asset-images"))
             <*> (Just <$> parseJSON s'')