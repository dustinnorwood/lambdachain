{-# LANGUAGE DerivingStrategies, FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecordWildCards, RecursiveDo, ScopedTypeVariables,TypeFamilies, TupleSections #-}
module Frontend.Login where

import Reflex.Dom.Core

import           Blockchain.Data.Address
import           Blockchain.Data.ExtendedWord
import           Blockchain.Data.Keccak256
import           Blockchain.Data.Keys
import           Blockchain.Data.RLP
import           Blockchain.Data.Signed
import           Blockchain.Data.Subject
import           Blockchain.Data.SubjectAndCert
import           Crypto.Hash.Keccak
import           Common.Utils
import           Control.Applicative ((<|>))
import           Control.Lens        ((<&>))
import           Control.Monad (join)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Bifunctor (first)
import           Data.Bits              (shiftR, (.&.))
import           Data.Bool              (bool)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import           Data.Char (chr)
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text    as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Word
import           Language.Javascript.JSaddle
import           Obelisk.Frontend.Storage
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (SetRoute, setRoute)
import           Text.Read              (readMaybe)

import           Common.Route
import           Frontend.Client (urlGET, urlPOST)
import           Frontend.Utils  (buttonClass, dynButtonClass)
import Crypto.Hash.Keccak (keccak256Rate)

newtype Certificate = Certificate {
  commonName :: Text
} deriving stock (Eq, Show)

instance FromJSON Certificate where
  parseJSON = withObject "Certificate" $ \o ->
    Certificate <$> (fromMaybe "" <$> (o .:? "commonName"))

hexNibble :: Word8 -> Word8
hexNibble 0x0 = 0x30
hexNibble 0x1 = 0x31
hexNibble 0x2 = 0x32
hexNibble 0x3 = 0x33
hexNibble 0x4 = 0x34
hexNibble 0x5 = 0x35
hexNibble 0x6 = 0x36
hexNibble 0x7 = 0x37
hexNibble 0x8 = 0x38
hexNibble 0x9 = 0x39
hexNibble 0xa = 0x61
hexNibble 0xb = 0x62
hexNibble 0xc = 0x63
hexNibble 0xd = 0x64
hexNibble 0xe = 0x65
hexNibble 0xf = 0x66

hex :: Word8 -> [Word8]
hex w = [hexNibble $ (w `shiftR` 4) .&. 0xf, hexNibble $ w .&. 0xf]

login
  :: ( DomBuilder t m
     , Prerender t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
login = divClass "login-page" $ do
  divClass "login-title" . el "h1" $ text "LambdaChain"
  divClass "login-container" $ mdo
    el "h2" $ text "Login"
    (tUsername, tPassword) <- el "form" $ do
      tUsername' <- inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
          ("placeholder" =: "Username" <> "type" =: "text")
      divClass "error" $ dynText $ maybe "Account not found" (const "") <$> mKeyExistsD
      tPassword' <- inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
          ("placeholder" =: "Password" <> "type" =: "password")
      divClass "error" $ dynText $ maybe "" (const "Incorrect password") <$> mPasswordMismatchD
      pure (tUsername', tPassword')
    clickEv <- buttonClass "option-full" "Login"
    el "br" blank
    divClass "info" $ do
      el "div" $ text "Don't have an account?"
      el "div" $ elAttr "a" ("href" =: "/register") $ text "Register instead."
    let usernameEv = tag (current $ value tUsername) clickEv
    mKeyExistsE :: Event t (Text, Maybe KeyInfo) <- getLocalStorageItem $ ("key_for_" <>) <$> usernameEv
    mKeyExistsD <- holdDyn (Just $ KeyInfo emptyHash zeroAddress) $ snd <$> mKeyExistsE
    let keyExistsE = fmapMaybe id $ sequence <$> mKeyExistsE
    keyExistsEDyn <- holdDyn Nothing $ Just <$> keyExistsE
    let usernameExistsE = fst <$> keyExistsE
    usernameExistsEDyn <- holdDyn Nothing $ Just <$> usernameExistsE
    let passAndEncPriv = (\(p,(u, KeyInfo k a)) -> (p,u,k,a)) <$> attach (current $ value tPassword) keyExistsE
    passAndEncPrivDyn <- holdDyn Nothing $ Just <$> passAndEncPriv
    let decPriv = (\(p,u,Keccak256 k,a) -> (u, decryptPrivateKey p k, a)) <$> passAndEncPriv
    decPrivDyn <- holdDyn Nothing $ Just <$> decPriv
    let getAddress pkE = fmap join . prerender (pure $ constDyn Nothing) $ do
          let getAddr = fmap publicKeyToAddress . liftJSM . toPublicKey
          widgetHold (pure Nothing) ((\(u,k,a) -> Just . (u,k,) . (a,) <$> getAddr k) <$> pkE)
    addrDyn <- getAddress decPriv
    let addrE = fmapMaybe id $ updated addrDyn
    let passwordMismatchE = fmapMaybe (\(_,_,(a,a')) -> bool (Just ()) Nothing (a == a')) addrE
    mPasswordMismatchD <- holdDyn Nothing $ Just <$> passwordMismatchE
    let passwordMatchE = fmapMaybe (\(u,k,(a,a')) -> bool Nothing (Just $ LoginInfo (T.drop 8 u) k) (a == a')) addrE
    storageEv <- putSessionStorageItem $ (,) "login_info" <$> passwordMatchE
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
register = divClass "login-page" $ do
  divClass "login-title" . el "h1" $ text "LambdaChain"
  divClass "login-container" $ mdo
    el "h2" $ text "Registration"
    (tUsername, tPassword) <- el "form" $ do
      tUsername' <- inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
          ("placeholder" =: "Username" <> "type" =: "text")
      divClass "error" $ dynText $ maybe "" (const "Username already exists") <$> (uncurry (<|>) <$> zipDyn (fmap (const ()) <$> searchResults) (fmap (const ()) <$> mKeyExistsD))
      tPassword' <- inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
          ("placeholder" =: "Password" <> "type" =: "password")
      divClass "error" $ text ""
      pure (tUsername', tPassword')
    clickedDyn <- holdDyn False $ leftmost [True <$ clickEv, False <$ postCertResultEv, False <$ fmapMaybe id (updated (uncurry (<|>) <$> zipDyn (fmap (const ()) <$> searchResults) (fmap (const ()) <$> mKeyExistsD)))]
    mClickEv <- dynButtonClass "option-full" clickedDyn $ bool "Register" "Registering..." <$> clickedDyn
    let clickEv = fmapMaybe (bool (Just ()) Nothing . T.null) $ tag (current $ value tUsername) mClickEv
    el "br" blank
    divClass "info" $ do
      el "div" $ text "Already have an account?"
      el "div" $ elAttr "a" ("href" =: "/login") $ text "Login instead."
    let srch = value tUsername
        isEmpty s | s == "" = Nothing
                  | otherwise  = Just s
    srchEv <- debounce 0.5 . fmapMaybe isEmpty $ updated srch
    (searchResultsE :: Event t (Maybe [Certificate])) <- urlGET $ ("https://lambdachain.xyz/cirrus/search/Certificate?select=commonName&commonName=eq." <>) <$> srchEv
    searchResults <- holdDyn Nothing (maybe Nothing listToMaybe <$> searchResultsE)
    let usernameEv = tag (current $ value tUsername) clickEv
    mKeyExistsE :: Event t (Text, Maybe KeyInfo) <- getLocalStorageItem $ ("key_for_" <>) <$> srchEv
    mKeyExistsD <- holdDyn Nothing $ snd <$> mKeyExistsE
    let mKeyDoesntExistE = (\(mki, u) -> maybe (Just u) (const Nothing) mki) <$> attach (current mKeyExistsD) usernameEv
    mPrivKey <- fmap join . prerender (pure $ constDyn Nothing) $ widgetHold (pure Nothing) $ mKeyDoesntExistE <&> \case
      Nothing -> pure Nothing
      Just k -> do
        privKey <- liftJSM newPrivateKey
        pure $ Just (k, privKey)
    let privKeyE = fmapMaybe id $ updated mPrivKey
    let attachAddress pkE = fmap join . prerender (pure $ constDyn Nothing) $ do
          let getAddr = fmap publicKeyToAddress . liftJSM . toPublicKey
          widgetHold (pure Nothing) ((\(u,pk) -> Just . (u,) . (pk,) <$> getAddr pk) <$> pkE)
    uPkAddrE <- fmapMaybe id . updated <$> attachAddress privKeyE
    let passAndPriv = (\(p,(u,(k,a))) -> (p,u,k,a)) <$> attach (current $ value tPassword) uPkAddrE
        encPriv = (\(p,u,k,a) -> (p,u, encryptPrivateKey p k, a)) <$> passAndPriv
    mPassAndPriv <- holdDyn Nothing $ Just <$> passAndPriv
    mEncPriv <- holdDyn Nothing $ Just <$> encPriv
    postCertResultEv :: Event t (Maybe Text) <- fmap switchDyn . prerender (pure never) $ do
      let getSignature cn pk = do
            pub <- liftJSM $ toPublicKey pk
            let sub = SubjectAndCert (Subject (T.unpack cn) "" Nothing Nothing pub) Nothing
            sig <- liftJSM $ rlpSign pk sub
            pure . Just $ Signed sub sig
      mSignedSubDyn <- widgetHold (pure Nothing) $ uncurry getSignature <$> privKeyE
      urlPOST $ (,) "https://id.lambdachain.xyz/identity" <$> fmapMaybe id (updated mSignedSubDyn)
    keyE <- fmap (\(u,v) -> (T.drop 8 u, v)) <$> putLocalStorageItem ((\(_,u,k,a) -> ("key_for_" <> u,KeyInfo (Keccak256 k) a)) <$> tag (fromJust <$> current mEncPriv) postCertResultEv)
    _ <- putSessionStorageItem $ (,) "login_info" . (\(p,u,k,a) -> LoginInfo (T.drop 8 u) k) <$> tag (fromJust <$> current mPassAndPriv) keyE
    setRoute $ (FrontendRoute_Home :/ ()) <$ fmapMaybe id postCertResultEv
    pure ()

logout
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender t m
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
logout = divClass "container" . divClass "item" $ mdo
  clickEv <- buttonClass "option-full" "Logout"
  logoutEv <- removeSessionStorageItem $ "login_info" <$ clickEv
  setRoute $ (FrontendRoute_Login :/ ()) <$ logoutEv
  pure ()
