{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Obelisk.Frontend.Storage where

import           Common.Utils
import           Control.Monad              (join)
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import           GHCJS.DOM                  (currentWindowUnchecked)
import           GHCJS.DOM.Window           (getLocalStorage)
import           GHCJS.DOM.Storage          (getItem, removeItem, setItem)
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core
import           Text.Read                  (readMaybe)

getStorageItem
  :: ( DomBuilder t m
     , Prerender t m
     , Read a
     )
  => Event t Text -> m (Event t (Text, Maybe a))
getStorageItem keyEv = fmap switchDyn . prerender (pure never) $ do
  let getStorage k = liftJSM $ fmap (readMaybe =<<) $ (\s -> getItem s (T.unpack k)) =<< getLocalStorage =<< currentWindowUnchecked
  performEvent $ (\k -> (,) k <$> getStorage k) <$> keyEv
  -- fmap (fmapMaybe id . updated . join) . prerender (pure $ constDyn Nothing) $ do
  -- let getStorage k = liftJSM $ fmap (readMaybe =<<) $ (\s -> getItem s (T.unpack k)) =<< getLocalStorage =<< currentWindowUnchecked
  -- widgetHold (pure Nothing) ((\k -> Just . (,) k <$> getStorage k) <$> keyEv)

putStorageItem
  :: ( DomBuilder t m
     , Prerender t m
     , Show a
     )
  => Event t (Text, a) -> m (Event t (Text, a))
putStorageItem kvEv = fmap (fmapMaybe id . updated . join) . prerender (pure $ constDyn Nothing) $ do
  let putStorage k v = liftJSM $ (\s -> setItem s (T.unpack k) (show v)) =<< getLocalStorage =<< currentWindowUnchecked
  widgetHold (pure Nothing) ((\(k,v) -> Just (k,v) <$ putStorage k v) <$> kvEv)

removeStorageItem
  :: ( DomBuilder t m
     , Prerender t m
     )
  => Event t Text -> m (Event t Text)
removeStorageItem keyEv = fmap (fmapMaybe id . updated . join) . prerender (pure $ constDyn Nothing) $ do
  let removeStorage k = liftJSM $ (\s -> k <$ removeItem s (T.unpack k)) =<< getLocalStorage =<< currentWindowUnchecked
  widgetHold (pure Nothing) ((\k -> Just k <$ removeStorage k) <$> keyEv)