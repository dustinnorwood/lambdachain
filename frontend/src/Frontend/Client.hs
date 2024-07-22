{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Frontend.Client where

import Control.Lens
import Reflex
import Reflex.Dom.Core hiding (Namespace)

import Control.Monad           (join)
import Data.Aeson              (decode, ToJSON, FromJSON)
import Data.Dependent.Sum      (DSum(..))
import Data.Functor.Identity   (Identity (..))
import Data.Map.Strict         (Map)
import Data.Text               (Text)
import Data.Text.Lazy          (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Reflex.Dom.Xhr          (xhrResponse_responseText, xhrResponse_status)
import Servant.API             (NoContent)

import           Common.Api.Errors                 (ErrorBody)
import           Common.Api.Namespace              (Namespace)
import           Common.Api.Validation             (ValidationErrors)
import           Common.Route
import           Obelisk.Route                     hiding (decode)

type ClientRes t a = (Event t a, Event t ClientError, Dynamic t Bool)

data ClientError
  = Forbidden
  | NotFound
  | Unauthorised
  | FailedValidation (Maybe (ErrorBody ValidationErrors))
  | OtherError Word Text
  deriving (Show)

-- urlGET ::
--   (MonadHold t m, PostBuild t m, Prerender t m, FromJSON b) =>
--   Dynamic t Text -> m (Event t (Maybe b))
-- urlGET urlDyn = fmap switchDyn $ prerender (pure never) $ do
--   pb <- getPostBuild
--   getAndDecode $ leftmost [updated urlDyn, tagPromptlyDyn urlDyn pb]

-- backendGET ::
--   (MonadHold t m, PostBuild t m, Prerender t m, FromJSON b) =>
--   Dynamic t (DSum BackendRoute Identity) -> m (Event t (Maybe b))
-- backendGET routeDyn = urlGET urlDyn
--   where
--     urlDyn = renderBackendRoute enc <$> routeDyn
--     Right (enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName) = checkEncoder fullRouteEncoder

urlGET ::
  (MonadHold t m, PostBuild t m, Prerender t m, FromJSON b) =>
  Event t Text -> m (Event t (Maybe b))
urlGET urlEv = fmap switchDyn $ prerender (pure never) $ do
  getAndDecode urlEv

urlGETDyn ::
  (MonadHold t m, PostBuild t m, Prerender t m, FromJSON b) =>
  Dynamic t Text -> m (Event t (Maybe b))
urlGETDyn urlDyn = fmap switchDyn $ prerender (pure never) $ do
  pb <- getPostBuild
  getAndDecode $ leftmost [updated urlDyn, tagPromptlyDyn urlDyn pb]

-- backendGETEv ::
--   (MonadHold t m, PostBuild t m, Prerender t m, FromJSON b) =>
--   Event t (DSum BackendRoute Identity) -> m (Event t (Maybe b))
-- backendGETEv routeEv = urlGETEv urlEv
--   where
--     urlEv = renderBackendRoute enc <$> routeEv
--     Right (enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName) = checkEncoder fullRouteEncoder

-- | Simplified interface to "POST" URLs and return decoded results.
postAndDecode :: (ToJSON a, FromJSON b, MonadWidget t m) => Event t (Text, a) -> m (Event t (Maybe b))
postAndDecode urlAndBody = do
  r <- performRequestAsync $ fmap (uncurry postJson) urlAndBody
  return $ fmap decodeXhrResponse r

urlPOST ::
  (MonadHold t m, PostBuild t m, Prerender t m, ToJSON a, FromJSON b) =>
  Event t (Text, a) -> m (Event t (Maybe b))
urlPOST = fmap switchDyn . prerender (pure never) . postAndDecode

-- backendPOST ::
--   (MonadHold t m, PostBuild t m, Prerender t m, ToJSON a, FromJSON b) =>
--   Dynamic t (DSum BackendRoute Identity) -> Dynamic t a -> m (Event t (Maybe b))
-- backendPOST routeDyn = urlPOST urlDyn
--   where
--     urlDyn = renderBackendRoute enc <$> routeDyn
--     Right (enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName) = checkEncoder fullRouteEncoder
-- 
-- backendPostEvent ::
--   (MonadHold t m, PostBuild t m, Prerender t m, ToJSON a, FromJSON b) =>
--   Event t (DSum BackendRoute Identity, a) -> m (Event t (Maybe b))
-- backendPostEvent routeDyn = fmap switchDyn . prerender (pure never) $ postAndDecode urlDyn
--   where
--     urlDyn = (\(r, b) -> (renderBackendRoute enc r, b)) <$> routeDyn
--     Right (enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName) = checkEncoder fullRouteEncoder