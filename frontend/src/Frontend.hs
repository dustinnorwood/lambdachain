{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms, RankNTypes #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TypeApplications                                #-}

module Frontend where

import           Blockchain.Data.Keys
import           Common.Route
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Obelisk.Frontend           (Frontend (Frontend), ObeliskWidget)
import           Obelisk.Frontend.Storage   (getStorageItem)
import           Obelisk.Route.Frontend     (R, pattern (:/), RoutedT, setRoute, subRoute)
import           Reflex.Dom.Core            hiding (Namespace)

import           Frontend.Account                (account)
import           Frontend.Head                   (htmlHead)
import           Frontend.HomePage               (homePage)
import           Frontend.ItemDetail             (itemDetail)
import           Frontend.Login                  (login, register)
import           Frontend.Nav                    (nav)
import           Frontend.Shop                   (shop)
import           Frontend.Tokens                 (tokenBalance)

htmlBody
  :: forall t m
  . ( ObeliskWidget t (R FrontendRoute) m
    )
  => RoutedT t (R FrontendRoute) m ()
htmlBody = mdo
  (mCreds :: Dynamic t (Event t (Maybe (Text, PrivateKey)))) <- prerender (pure never) $ do
    pbE <- getPostBuild
    let getCredsE = leftmost [txE, pbE]
    mUsernameE <- fmap snd <$> getStorageItem ("mercata_username" <$ getCredsE)
    fmap (\(k,mv) -> (,) (T.drop 8 k) <$> mv) <$> getStorageItem (("key_for_" <>) <$> fmapMaybe id mUsernameE)
  mCredsDyn <- holdDyn Nothing $ switchDyn mCreds
  txE <- fmap switchDyn . elClass "div" "grid-container" $ do
    elClass "div" "header" $ do
      (e,_) <- elAttr' "div" ("class" =: "header-title") $ do
        el "h1" $ text "LambdaChain"
        elAttr "div" ("style" =: "font-weight: bold;") $ el "h1" $ text ""
      -- divClass "header-title-bold" . el' "h1" $ text "λ"
      setRoute $ (FrontendRoute_Home :/ ()) <$ domEvent Click e
    el "br" blank
    el "br" blank
    elClass "div" "main" $ subRoute (pages mCredsDyn)
  pure ()
  where
    pages 
      :: Dynamic t (Maybe (Text, PrivateKey))
      -> FrontendRoute a
      -> RoutedT t a m (Event t ())
    pages mCreds rte = case rte of
      FrontendRoute_Home     -> do
        let loggedOutEv = fmapMaybe id $ maybe (Just ()) (const Nothing) <$> updated mCreds
        setRoute $ (FrontendRoute_Login :/ ()) <$ loggedOutEv
        nav 0
        homePage mCreds
        pure never
      FrontendRoute_Shop     -> do
        let loggedOutEv = fmapMaybe id $ maybe (Just ()) (const Nothing) <$> updated mCreds
        setRoute $ (FrontendRoute_Login :/ ()) <$ loggedOutEv
        txE <- shop mCreds
        nav 1
        pure txE
      FrontendRoute_Account     -> do
        account
        nav 2
        pure never
      FrontendRoute_ItemDetail -> do
        itemDetail mCreds
        nav 1
        pure never
      FrontendRoute_Search   -> never <$ blank
      FrontendRoute_Login    -> never <$ login
      FrontendRoute_Register -> never <$ register
      FrontendRoute_CompleteOrder -> do
        pBE <- getPostBuild
        setRoute $ (FrontendRoute_Home :/ ()) <$ pBE
        pure never

frontend :: Frontend (R FrontendRoute)
frontend = Frontend (prerender_ htmlHead htmlHead) htmlBody