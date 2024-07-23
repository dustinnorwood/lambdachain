{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Obelisk.Frontend.Storage where

import qualified Data.Text                  as T
import           Data.Text                  (Text)
import           GHCJS.DOM                  (currentWindowUnchecked)
import           GHCJS.DOM.Window           (getLocalStorage)
import           GHCJS.DOM.Storage          (getItem, removeItem, setItem)
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

getStorageItem
  :: ( DomBuilder t m
     , Prerender t m
     , Read a
     )
  => Event t Text -> m (Event t (Text, Maybe a))
getStorageItem keyEv = do
  uDynEv <- prerender (pure never) $ do
    let getStorage k = liftJSM $ fmap (fmap read) $ (\s -> getItem s (T.unpack k)) =<< getLocalStorage =<< currentWindowUnchecked
    d <- holdDyn (pure Nothing) ((\k -> Just . (,) k <$> getStorage k) <$> keyEv)
    e <- dyn d
    pure $ fmapMaybe id e
  pure $ switchDyn uDynEv

putStorageItem
  :: ( DomBuilder t m
     , Prerender t m
     , Show a
     )
  => Event t (Text, a) -> m (Event t (Text, a))
putStorageItem kvEv = do
  uDynEv <- prerender (pure never) $ do
    let putStorage k v = liftJSM $ (\s -> setItem s (T.unpack k) (show v)) =<< getLocalStorage =<< currentWindowUnchecked
    d <- holdDyn (pure Nothing) $ (\(k,v) -> Just (k,v) <$ putStorage k v) <$> kvEv
    e <- dyn d
    pure $ fmapMaybe id e
  pure $ switchDyn uDynEv

removeStorageItem
  :: ( DomBuilder t m
     , Prerender t m
     )
  => Event t Text -> m (Event t Text)
removeStorageItem keyEv = do
  uDynEv <- prerender (pure never) $ do
    let removeStorage k = liftJSM $ (\s -> k <$ removeItem s (T.unpack k)) =<< getLocalStorage =<< currentWindowUnchecked
    d <- holdDyn (pure Nothing) ((\k -> Just k <$ removeStorage k) <$> keyEv)
    e <- dyn d
    pure $ fmapMaybe id e
  pure $ switchDyn uDynEv