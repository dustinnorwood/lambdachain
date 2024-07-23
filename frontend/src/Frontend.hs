{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms, RankNTypes #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TupleSections, TypeApplications                 #-}

module Frontend where

import           Blockchain.Data.ExtendedWord
import           Blockchain.Data.Keys
import           Common.Route
import           Common.Utils
import           Control.Lens               ((<&>))
import           Control.Monad              (join, (<=<))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Language.Javascript.JSaddle
import           Obelisk.Frontend           (Frontend (Frontend), ObeliskWidget)
import           Obelisk.Frontend.Storage   (getSessionStorageItem)
import           Obelisk.Route.Frontend     (R, pattern (:/), RoutedT, setRoute, subRoute_)
import           Reflex.Dom.Core            hiding (Namespace)

import           Frontend.Account                (account)
import           Frontend.Head                   (htmlHead)
import           Frontend.HomePage               (homePage)
import           Frontend.ItemDetail             (itemDetail)
import           Frontend.Login                  (login, register)
import           Frontend.Nav                    (nav)
import           Frontend.Shop                   (shop)
import           Frontend.Tokens                 (tokenBalance)

headerWidget
  :: forall t m
  . ( ObeliskWidget t (R FrontendRoute) m
    )
  => m ()
headerWidget = elClass "div" "grid-container" $ do
  elClass "div" "header" $ do
    (e,_) <- elAttr' "div" ("class" =: "header-title") $ do
      el "h1" $ text "LambdaChain"
      -- elAttr "div" ("style" =: "font-weight: bold;") $ el "h1" $ text ""
    -- divClass "header-title-bold" . el' "h1" $ text "λ"
    setRoute $ (FrontendRoute_Home :/ ()) <$ domEvent Click e


getLoginInfo
  :: forall t m
  . ( ObeliskWidget t (R FrontendRoute) m
    )
  => m (Event t LoginInfo)
getLoginInfo = fmap switchDyn . prerender (pure never) $ do
  pBE <- getPostBuild
  mLoginInfoE :: Event t (Maybe LoginInfo) <- fmap snd <$> getSessionStorageItem ("login_info" <$ pBE)
  let notLoggedInE = fmapMaybe (maybe (Just ()) (const Nothing)) mLoginInfoE
  setRoute $ (FrontendRoute_Login :/ ()) <$ notLoggedInE
  pure $ fmapMaybe id mLoginInfoE

loadingWidget
  :: forall t m
  . ( ObeliskWidget t (R FrontendRoute) m
    )
  => m ()
loadingWidget = blank -- divClass "login-page" blank

htmlBody
  :: forall t m
  . ( ObeliskWidget t (R FrontendRoute) m
    )
  => RoutedT t (R FrontendRoute) m ()
htmlBody = do
  divClass "main" $ subRoute_ pages
  pure ()
  where
    pages 
      :: FrontendRoute a
      -> RoutedT t a m ()
    pages rte = case rte of
      FrontendRoute_Home     -> do
        loginInfoE <- getLoginInfo
        widgetHold_ loadingWidget $ loginInfoE <&> \loginInfo -> do
          headerWidget
          el "br" blank
          el "br" blank
          homePage loginInfo
          nav 0
          pure ()
      FrontendRoute_Shop     -> do
        loginInfoE <- getLoginInfo
        widgetHold_ loadingWidget $ loginInfoE <&> \loginInfo -> do
          headerWidget
          el "br" blank
          el "br" blank
          _ <- shop loginInfo
          nav 1
          pure ()
      FrontendRoute_Account     -> do
        loginInfoE <- getLoginInfo
        widgetHold_ loadingWidget $ loginInfoE <&> \_ -> do
          headerWidget
          el "br" blank
          el "br" blank
          account
          nav 2
          pure ()
      FrontendRoute_ItemDetail -> do
        loginInfoE <- getLoginInfo
        widgetHold_ loadingWidget $ loginInfoE <&> \_ -> do
          headerWidget
          el "br" blank
          el "br" blank
          itemDetail
          nav 1
          pure ()
      FrontendRoute_Search   -> blank
      FrontendRoute_Login    -> login
      FrontendRoute_Register -> register
      FrontendRoute_CompleteOrder -> do
        pBE <- getPostBuild
        setRoute $ (FrontendRoute_Home :/ ()) <$ pBE
        pure ()

frontend :: Frontend (R FrontendRoute)
frontend = Frontend (prerender_ htmlHead htmlHead) htmlBody