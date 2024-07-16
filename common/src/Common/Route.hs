{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

import           Prelude hiding (id, (.))
import           Blockchain.Data.Address
import           Common.Windowed
import           Control.Category
import           Control.Lens hiding (bimap)
import           Control.Monad (join)
import           Data.Dependent.Sum (DSum)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read (readMaybe)

import           Obelisk.Route
import           Obelisk.Route.TH

class RDefault a where
  rdef :: a

instance RDefault a => RDefault (Windowed a) where
  rdef = Windowed Nothing Nothing rdef

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()

tshow :: Show a => a -> Text
tshow = T.pack . show

queryEncoder :: (Applicative check, Applicative parse, Read a, Show a) => Encoder check parse (Maybe a) Text
queryEncoder = unsafeMkEncoder $ EncoderImpl
 (pure . readMaybe . T.unpack)
 (tshow)

mkQueryList :: [(Text, Maybe Text)] -> Map Text (Maybe Text)
mkQueryList = M.fromList . catMaybes . map (\(t, m) -> (t,) . Just <$> m)

windowedEncoder :: Encoder Identity Identity           decoded  (Map Text (Maybe Text))
                -> Encoder Identity Identity (Windowed decoded) (Map Text (Maybe Text))
windowedEncoder itemEncoder = unsafeMkEncoder $ EncoderImpl
  (\m -> Identity $ Windowed (decode queryEncoder . fromMaybe "" . join $ M.lookup "limit" m)
                             (decode queryEncoder . fromMaybe "" . join $ M.lookup "offset" m)
                             (decode itemEncoder m))
  (\(Windowed l o i) -> M.union (mkQueryList [("limit", tshow <$> l), ("offset", tshow <$> o)]) (encode itemEncoder i))

emptyWindowed :: Windowed Text
emptyWindowed = Windowed Nothing Nothing ""

homeRoute :: DSum FrontendRoute Identity
homeRoute = FrontendRoute_Home :/ ()

getItemsParamsEncoder :: (Applicative check, Applicative parse) => Encoder check parse Text (Map Text (Maybe Text))
getItemsParamsEncoder = unsafeMkEncoder $ EncoderImpl
  (\m -> pure $ fromMaybe "" $ join $ M.lookup "name" m)
  (\n -> mkQueryList [("name", Just n)])

getItemsEncoder :: Encoder Identity Identity (Windowed Text) (Map Text (Maybe Text))
getItemsEncoder = windowedEncoder getItemsParamsEncoder

searchItemsParamsEncoder :: (Applicative check, Applicative parse) => Encoder check parse Text (Map Text (Maybe Text))
searchItemsParamsEncoder = unsafeMkEncoder $ EncoderImpl
  (pure . fromMaybe "" . join . M.lookup "search")
  (\s -> mkQueryList [("search", Just s)])

searchItemsEncoder :: Encoder Identity Identity (Windowed Text) (Map Text (Maybe Text))
searchItemsEncoder = windowedEncoder searchItemsParamsEncoder

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Shop :: FrontendRoute ()
  FrontendRoute_Account :: FrontendRoute ()
  FrontendRoute_Search :: FrontendRoute (Windowed Text)
  FrontendRoute_Login :: FrontendRoute ()
  FrontendRoute_Register :: FrontendRoute ()
  FrontendRoute_Settings :: FrontendRoute ()
  FrontendRoute_ItemDetail :: FrontendRoute Address

data ProfileRoute :: * -> * where
  ProfileRoute_Favorites :: ProfileRoute ()

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing           -> PathSegment "missing" $ unitEncoder mempty
  )
  (\case
      FrontendRoute_Home -> PathEnd $ unitEncoder mempty
      FrontendRoute_Shop -> PathSegment "shop" $ unitEncoder mempty
      FrontendRoute_Account -> PathSegment "account" $ unitEncoder mempty
      FrontendRoute_Search -> PathSegment "search"
                            $ queryOnlyEncoder .
                            ( hoistParse (pure . runIdentity)
                            . hoistCheck (pure . runIdentity)
                            $ searchItemsEncoder
                            )
      FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
      FrontendRoute_Register -> PathSegment "register" $ unitEncoder mempty
      FrontendRoute_Settings -> PathSegment "settings" $ unitEncoder mempty
      FrontendRoute_ItemDetail -> PathSegment "item" $ singlePathSegmentEncoder . addressEncoder
  )

checkedEncoder :: Applicative check => Encoder check Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedEncoder = either (error "checkEncoder failed") id $ checkEncoder fullRouteEncoder

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
