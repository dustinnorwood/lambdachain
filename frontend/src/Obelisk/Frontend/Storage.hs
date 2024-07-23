{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Obelisk.Frontend.Storage where

import           Data.Aeson                 (ToJSON, FromJSON, decode, encode)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           GHCJS.DOM                  (currentWindowUnchecked)
import           GHCJS.DOM.Window           (Window, getLocalStorage, getSessionStorage)
import           GHCJS.DOM.Storage          (Storage, getItem, removeItem, setItem)
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core            hiding (Window)
import           Text.Read                  (readMaybe)

getLocalStorageItem
  :: ( DomBuilder t m
     , Prerender t m
     , FromJSON a
     )
  => Event t Text -> m (Event t (Text, Maybe a))
getLocalStorageItem = getStorageItemFrom getLocalStorage

putLocalStorageItem
  :: ( DomBuilder t m
     , Prerender t m
     , ToJSON a
     )
  => Event t (Text, a) -> m (Event t (Text, a))
putLocalStorageItem = putStorageItemFrom getLocalStorage

removeLocalStorageItem
  :: ( DomBuilder t m
     , Prerender t m
     )
  => Event t Text -> m (Event t Text)
removeLocalStorageItem = removeStorageItemFrom getLocalStorage

getSessionStorageItem
  :: ( DomBuilder t m
     , Prerender t m
     , FromJSON a
     )
  => Event t Text -> m (Event t (Text, Maybe a))
getSessionStorageItem = getStorageItemFrom getSessionStorage

putSessionStorageItem
  :: ( DomBuilder t m
     , Prerender t m
     , ToJSON a
     )
  => Event t (Text, a) -> m (Event t (Text, a))
putSessionStorageItem = putStorageItemFrom getSessionStorage

removeSessionStorageItem
  :: ( DomBuilder t m
     , Prerender t m
     )
  => Event t Text -> m (Event t Text)
removeSessionStorageItem = removeStorageItemFrom getSessionStorage

getStorageItemFrom
  :: ( DomBuilder t m
     , Prerender t m
     , FromJSON a
     )
  => (Window -> JSM Storage) -> Event t Text -> m (Event t (Text, Maybe a))
getStorageItemFrom getStorageF keyEv = do
  uDynEv <- prerender (pure never) $ do
    let getStorage k = liftJSM $ fmap (decode . BL.fromStrict . encodeUtf8 . T.pack =<<) $ (\s -> getItem s (T.unpack k)) =<< getStorageF =<< currentWindowUnchecked
    d <- holdDyn (pure Nothing) ((\k -> Just . (,) k <$> getStorage k) <$> keyEv)
    e <- dyn d
    pure $ fmapMaybe id e
  pure $ switchDyn uDynEv

putStorageItemFrom
  :: ( DomBuilder t m
     , Prerender t m
     , ToJSON a
     )
  => (Window -> JSM Storage) -> Event t (Text, a) -> m (Event t (Text, a))
putStorageItemFrom getStorageF kvEv = do
  uDynEv <- prerender (pure never) $ do
    let putStorage k v = liftJSM $ (\s -> setItem s (T.unpack k) (T.unpack . decodeUtf8 . BL.toStrict $ encode v)) =<< getStorageF =<< currentWindowUnchecked
    d <- holdDyn (pure Nothing) $ (\(k,v) -> Just (k,v) <$ putStorage k v) <$> kvEv
    e <- dyn d
    pure $ fmapMaybe id e
  pure $ switchDyn uDynEv

removeStorageItemFrom
  :: ( DomBuilder t m
     , Prerender t m
     )
  => (Window -> JSM Storage) -> Event t Text -> m (Event t Text)
removeStorageItemFrom getStorageF keyEv = do
  uDynEv <- prerender (pure never) $ do
    let removeStorage k = liftJSM $ (\s -> k <$ removeItem s (T.unpack k)) =<< getStorageF =<< currentWindowUnchecked
    d <- holdDyn (pure Nothing) ((\k -> Just k <$ removeStorage k) <$> keyEv)
    e <- dyn d
    pure $ fmapMaybe id e
  pure $ switchDyn uDynEv