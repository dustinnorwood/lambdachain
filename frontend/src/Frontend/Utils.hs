{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Frontend.Utils where

import Control.Lens    hiding (element)
import Reflex.Dom.Core

import           Blockchain.Data.Address
import           Control.Applicative    (liftA2)
import           Control.Monad          (mfilter)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Trans    (lift)
import           Data.Bool              (bool)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import           Data.Proxy             (Proxy (Proxy))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Obelisk.Route.Frontend (RouteToUrl, RoutedT, SetRoute, askRoute, askRouteToUrl, runRoutedT,
                                         setRoute)

showText :: Show s => s -> Text
showText = T.pack . show

imgUrl :: Maybe Text -> Text
imgUrl =
  fromMaybe "https://static.productionready.io/images/smiley-cyrus.jpg"
  . mfilter (not . T.null . T.strip)

zipDynA :: (Applicative f, Reflex t) => Dynamic t (f a) -> Dynamic t (f b) -> Dynamic t (f (a,b))
zipDynA = zipDynWith $ liftA2 (,)

assetUrl :: Text
-- assetUrl = "https://lambdachain.xyz/cirrus/search/BlockApps-Mercata-Asset?limit=100&order=root&or=(data->>isMint.eq.true,quantity.gt.0)&select=*,BlockApps-Mercata-Sale!BlockApps-Mercata-Sale_BlockApps-Mercata-Asset_fk(*,BlockApps-Mercata-Sale-paymentProviders(*)),BlockApps-Mercata-Asset-images(*),BlockApps-Mercata-Bid(*)"
assetUrl = "https://lambdachain.xyz/cirrus/search/BlockApps-Mercata-Asset?limit=100&order=root&or=(data-%3E%3EisMint.eq.true,quantity.gt.0)&select=address,root,name,description,quantity,itemNumber,BlockApps-Mercata-Sale!BlockApps-Mercata-Sale_BlockApps-Mercata-Asset_fk(isOpen,address,price,quantity,BlockApps-Mercata-Sale-paymentProviders(value)),BlockApps-Mercata-Asset-images(value),BlockApps-Mercata-Bid(price,isOpen,address,quantity,purchaserCommonName)"
{-# NOINLINE assetUrl #-}

tokenPsAddr :: Address
tokenPsAddr = read "41705c1d27122bba01fc5a36a9b1c59508bb4b32"
{-# NOINLINE tokenPsAddr #-}

-- These should probably be in obelisk!

pathSegmentSubRoute :: (Monad m, Functor (Dynamic t)) => (Dynamic t a -> RoutedT t b m c) -> RoutedT t (a, b) m c
pathSegmentSubRoute f = do
  rDyn <- askRoute
  lift $ runRoutedT (f (Prelude.fst <$> rDyn)) (Prelude.snd <$> rDyn)

buttonDynClass
  :: forall t m a
  . (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> Dynamic t Bool
  -> m a
  -> m (Event t ())
buttonDynClass clsDyn disabledDyn m = do
  let attrsDyn = (<>) <$> (("class" =:) <$> clsDyn) <*> (bool (Map.empty) ("disabled" =: "") <$> disabledDyn)
  modAttrs <- dynamicAttributesToModifyAttributes attrsDyn
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_modifyAttributes .~ modAttrs
  (e, _) <- element "button" cfg m
  pure $ domEvent Click e

routeLinkClass
  :: forall t m a r
  .  ( DomBuilder t m
     , RouteToUrl r m
     , SetRoute t r m
     , PostBuild t m
     , MonadSample t m
     )
  => Text
  -> r
  -> m a
  -> m a
routeLinkClass c = routeLinkDynClass (constDyn c) . constDyn

routeLinkAttr
  :: forall t m a r
  .  ( DomBuilder t m
     , RouteToUrl r m
     , SetRoute t r m
     , PostBuild t m
     , MonadSample t m
     )
  => Map.Map AttributeName Text
  -> r
  -> m a
  -> m a
routeLinkAttr attrs = routeLinkDynAttr (constDyn attrs) . constDyn

routeLinkDyn
  :: forall t m a r
  .  ( DomBuilder t m
     , RouteToUrl r m
     , SetRoute t r m
     , PostBuild t m
     , MonadSample t m
     )
  => Dynamic t r
  -> m a
  -> m a
routeLinkDyn = routeLinkDynAttr (constDyn Map.empty)

routeLinkDynClass
  :: forall t m a r
  .  ( DomBuilder t m
     , RouteToUrl r m
     , SetRoute t r m
     , PostBuild t m
     , MonadSample t m
     )
  => Dynamic t Text
  -> Dynamic t r
  -> m a
  -> m a
routeLinkDynClass cDyn = routeLinkDynAttr (("class" =:) <$> cDyn)

routeLinkDynAttr
  :: forall t m a r
  .  ( DomBuilder t m
     , RouteToUrl r m
     , SetRoute t r m
     , PostBuild t m
     , MonadSample t m
     )
  => Dynamic t (Map.Map AttributeName Text)
  -> Dynamic t r
  -> m a
  -> m a
routeLinkDynAttr attrDyn rDyn m = do
  enc <- askRouteToUrl
  let attrsDyn = (Map.insert "href" . enc <$> rDyn <*> attrDyn)
  initAttrs <- sample . current $ attrsDyn
  modAttrs <- dynamicAttributesToModifyAttributesWithInitial initAttrs attrsDyn
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ initAttrs
        & elementConfig_modifyAttributes  .~ modAttrs
  (e, a) <- element "a" cfg m
  setRoute $ current rDyn <@ domEvent Click e
  return a

buttonClass :: (DomBuilder t m, PostBuild t m) => Text -> Text -> m (Event t ())
buttonClass class' buttonText = do
  (e, _) <- elAttr' "div" ("class" =: class') $ text buttonText
  pure $ () <$ domEvent Click e

selectableButton :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Text -> m (Event t ())
selectableButton isLoading buttonText = do
  (e, _) <- elDynAttr' "button" (bool Map.empty ("class" =: "selected") <$> isLoading) $ text buttonText
  pure $ () <$ domEvent Click e

selectableButtonClass :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Bool -> Text -> m (Event t ())
selectableButtonClass class' isLoading buttonText = do
  (e, _) <- elDynAttr' "button" (bool ("class" =: class') ("class" =: ("selected " <> class')) <$> isLoading) $ text buttonText
  pure $ () <$ domEvent Click e

dynButton :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Dynamic t Text -> m (Event t ())
dynButton isLoading buttonText = do
  (e, _) <- elDynAttr' "button" (bool Map.empty ("class" =: "disabled") <$> isLoading) $ dynText buttonText
  pure . fmapMaybe id $ bool (Just ()) Nothing <$> tag (current isLoading) (domEvent Click e)

dynButtonClass :: (DomBuilder t m, PostBuild t m) => Text -> Dynamic t Bool -> Dynamic t Text -> m (Event t ())
dynButtonClass class' isLoading buttonText = do
  (e, _) <- elDynAttr' "button" (bool ("class" =: class') ("class" =: ("disabled " <> class')) <$> isLoading) $ dynText buttonText
  pure . fmapMaybe id $ bool (Just ()) Nothing <$> tag (current isLoading) (domEvent Click e)

toggleWidget
  :: ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     )
  => m (Event t a) -> m (Event t b) -> m (Event t (Either a b)) 
toggleWidget leftWidget rightWidget = mdo
  isToggled <- holdDyn False $ fst <$> toggleE
  let left = fmap ((True,) . Left) <$> leftWidget
  toggleD <- widgetHold left . updated $ isToggled <&> \case
    True -> fmap ((False,) . Right) <$> rightWidget
    False -> left
  let toggleE = switchDyn toggleD
  pure $ snd <$> toggleE