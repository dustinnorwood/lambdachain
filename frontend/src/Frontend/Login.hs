{-# LANGUAGE DerivingStrategies, FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecordWildCards, RecursiveDo, ScopedTypeVariables,TypeFamilies, TupleSections #-}
module Frontend.Login where

import Reflex.Dom.Core

import           Blockchain.Data.Keys
import           Blockchain.Data.Signed
import           Blockchain.Data.Subject
import           Blockchain.Data.SubjectAndCert
import           Control.Applicative ((<|>))
import           Control.Monad (join)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text    as T
import           Language.Javascript.JSaddle
import           Obelisk.Frontend.Storage
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (SetRoute, setRoute)

import           Common.Route
import           Frontend.Client (urlGET, urlPOST)
import           Frontend.Utils  (buttonClass)

newtype Certificate = Certificate {
  commonName :: Text
} deriving stock (Eq, Show)

instance FromJSON Certificate where
  parseJSON = withObject "Certificate" $ \o ->
    Certificate <$> (fromMaybe "" <$> (o .:? "commonName"))

login
  :: ( DomBuilder t m
     , Prerender t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
login = divClass "container" . divClass "item" $ mdo
  el "h2" $ text "Login"
  tUsername <- el "form" $ do
    tUsername' <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Username" <> "type" =: "text")
    divClass "error" $ dynText $ maybe "Account not found" (const "") <$> mKeyExistsD
    _ <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Password" <> "type" =: "password")
    el "br" blank
    el "br" blank
    pure tUsername'
  clickEv <- elAttr "div" ("class" =: "button-container") $ buttonClass "option" "Login"
  el "br" blank
  divClass "info" $ do
    el "div" $ text "Don't have an account?"
    el "div" $ elAttr "a" ("href" =: "/register") $ text "Register instead."
  let usernameEv = tagPromptlyDyn (value tUsername) clickEv
  mKeyExistsE :: Event t (Text, Maybe Text) <- getStorageItem $ ("key_for_" <>) <$> usernameEv
  mKeyExistsD <- holdDyn (Just "") $ snd <$> mKeyExistsE
  let keyExistsE = fmapMaybe id $ fmap fst . sequence <$> mKeyExistsE
  storageEv <- putStorageItem $ (,) "mercata_username" . T.drop 8 <$> keyExistsE
  setRoute $ (FrontendRoute_Home :/ ()) <$ storageEv
  pure ()

register
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
  => m ()
register = divClass "container" . divClass "item" $ mdo
  el "h2" $ text "Register"
  tUsername <- el "form" $ do
    tUsername' <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Username" <> "type" =: "text")
    divClass "error" $ dynText $ maybe "" (const "Username already exists") <$> (uncurry (<|>) <$> zipDyn (fmap (const ()) <$> searchResults) (fmap (const ()) <$> mKeyExistsD))
    _ <- inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Password" <> "type" =: "password")
    el "br" blank
    el "br" blank
    pure tUsername'
  clickEv <- elDynAttr "div" (("class" =:) . maybe "button-container" (const "button-container disabled") <$> searchResults) $ buttonClass "option" "Register"
  el "br" blank
  divClass "info" $ do
    text "Already have an account?"
    elAttr "a" ("href" =: "/login") $ text "Login instead."
  let srch = value tUsername
      isEmpty s | s == "" = Nothing
                | otherwise  = Just s
  srchEv <- debounce 0.5 . fmapMaybe isEmpty $ updated srch
  (searchResultsE :: Event t (Maybe [Certificate])) <- urlGET $ ("https://lambdachain.xyz/cirrus/search/Certificate?select=commonName&commonName=eq." <>) <$> srchEv
  searchResults <- holdDyn Nothing (maybe Nothing listToMaybe <$> searchResultsE)
  let usernameEv = tagPromptlyDyn (value tUsername) clickEv
  (mKeyExistsE :: Event t (Text, Maybe String)) <- getStorageItem $ ("key_for_" <>) <$> usernameEv
  mKeyExistsD <- holdDyn Nothing $ snd <$> mKeyExistsE
  let mKeyDoesntExistE = (\(k, mv) -> maybe (Just k) (const Nothing) mv) <$> mKeyExistsE
  mPrivKeyD <- prerender (pure never) $ do
    privKey <- liftIO newPrivateKey
    pure $ fmap (flip (,) privKey) <$> mKeyDoesntExistE
  let privKeyE = fmapMaybe id $ switchDyn mPrivKeyD
  keyE <- putStorageItem privKeyE
  let usernameAndKeyE = (\(k,v) -> (T.drop 8 k, v)) <$> keyE
  _ <- putStorageItem $ (,) "mercata_username" . T.drop 8 . fst <$> keyE
  mSignedSubDyn <- fmap join . prerender (pure $ constDyn Nothing) $ do
    let getSignature cn pk = do
          pub <- liftJSM $ toPublicKey pk
          let sub = SubjectAndCert (Subject (T.unpack cn) "" Nothing Nothing pub) Nothing
          sig <- liftJSM $ rlpSign pk sub
          pure . Just $ Signed sub sig
    widgetHold (pure Nothing) $ uncurry getSignature <$> usernameAndKeyE
  postCertResultDyn <- prerender (pure never) $ do
    (postCertResult :: Event t (Maybe Certificate)) <- urlPOST $ (,) "https://id.lambdachain.xyz/identity" <$> fmapMaybe id (updated mSignedSubDyn)
    pure postCertResult
  setRoute $ (FrontendRoute_Home :/ ()) <$ fmapMaybe id (switchDyn postCertResultDyn)
  pure ()

logout
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender t m
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
logout = divClass "container" . divClass "item" $ mdo
  clickEv <- elAttr "div" ("class" =: "button-container") $ buttonClass "option" "Logout"
  logoutEv <- removeStorageItem $ "mercata_username" <$ clickEv
  setRoute $ (FrontendRoute_Login :/ ()) <$ logoutEv
  pure ()
