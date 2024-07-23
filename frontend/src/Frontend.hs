{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms, RankNTypes #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TypeApplications                                #-}

module Frontend where

import           Blockchain.Data.ExtendedWord
import           Blockchain.Data.Keys
import           Common.Route
import           Common.Utils
import           Control.Lens               ((<&>))
import           Control.Monad              (join, (<=<))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Obelisk.Frontend           (Frontend (Frontend), ObeliskWidget)
import           Obelisk.Frontend.Storage   (getStorageItem)
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


requireLoggedIn
  :: forall t m
  . ( ObeliskWidget t (R FrontendRoute) m
    )
  => m (Event t (Text, PrivateKey))
requireLoggedIn = fmap switchDyn . prerender (pure never) $ do
  pBE <- getPostBuild
  mUsernameE :: Event t (Maybe Text) <- fmap snd <$> getStorageItem ("mercata_username" <$ pBE)
  let noUsernameE = fmapMaybe (maybe (Just ()) (const Nothing)) mUsernameE
  let usernameE = fmapMaybe id mUsernameE
  mCreds :: Event t (Maybe (Text, PrivateKey)) <- fmap (\(k,mv) -> (,) (T.drop 8 k) <$> mv) <$> getStorageItem (("key_for_" <>) <$> usernameE)
  let noPrivateKeyE = fmapMaybe (maybe (Just ()) (const Nothing)) mCreds
  setRoute $ (FrontendRoute_Login :/ ()) <$ leftmost [noUsernameE, noPrivateKeyE]
  pure $ fmapMaybe id mCreds

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
        credsE <- requireLoggedIn
        widgetHold_ loadingWidget $ credsE <&> \creds -> do
          headerWidget
          el "br" blank
          el "br" blank
          homePage creds
          nav 0
          pure ()
      FrontendRoute_Shop     -> do
        credsE <- requireLoggedIn
        widgetHold_ loadingWidget $ credsE <&> \creds -> do
          headerWidget
          el "br" blank
          el "br" blank
          _ <- shop creds
          nav 1
          pure ()
      FrontendRoute_Account     -> do
        credsE <- requireLoggedIn
        widgetHold_ loadingWidget $ credsE <&> \_ -> do
          headerWidget
          el "br" blank
          el "br" blank
          account
          nav 2
          pure ()
      FrontendRoute_ItemDetail -> do
        credsE <- requireLoggedIn
        widgetHold_ loadingWidget $ credsE <&> \_ -> do
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