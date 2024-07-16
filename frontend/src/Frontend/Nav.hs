{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecordWildCards, RecursiveDo, TypeFamilies, TupleSections #-}
module Frontend.Nav where

import Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import Control.Monad.IO.Class
import Obelisk.Route          (pattern (:/), R)
import Obelisk.Route.Frontend (SetRoute, setRoute)

import           Common.Route

nav
  :: ( DomBuilder t m
     , PerformEvent t m
     , Prerender t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , SetRoute t (R FrontendRoute) m
     )
  => Int -> m ()
nav i = elAttr "nav" ("class" =: "navbar") $ do
  elAttr "div" ("class" =: ("nav-item" <> if i == 0 then " active" else "")) $ do
    (e, _) <- el' "div" $ text "Home"
    setRoute $ (FrontendRoute_Home :/ ()) <$ domEvent Click e
  elAttr "div" ("class" =: ("nav-item" <> if i == 1 then " active" else "")) $ do
    (e, _) <- el' "div" $ text "Shop"
    setRoute $ (FrontendRoute_Shop :/ ()) <$ domEvent Click e
  elAttr "div" ("class" =: ("nav-item" <> if i == 2 then " active" else "")) $ do
    (e, _) <- el' "div" $ text "Account"
    setRoute $ (FrontendRoute_Account :/ ()) <$ domEvent Click e
  pure ()