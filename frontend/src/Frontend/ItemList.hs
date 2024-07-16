{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.ItemList where

import           Common.Route
import           Control.Applicative    (liftA2)
import           Control.Monad          (join)
import           Control.Monad.Fix      (MonadFix)
import           Data.Bool              (bool)
import           Data.Functor           ((<&>))
import qualified Data.Map.Strict        as M
import           Data.Maybe             (isJust)
import qualified Data.Text              as T
import           Frontend.Utils         (dynButtonClass, selectableButtonClass, buttonClass)
import           Mercata.Data.Item
import           Mercata.Data.Trade
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (SetRoute, setRoute)
import           Reflex.Dom.Core
import           Text.Printf
import           Text.Read              (readMaybe)

itemList
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , SetRoute t (R FrontendRoute) m
     )
  => TradeAction -> Dynamic t (Maybe Int) -> [Item] -> m (Dynamic t [Event t ((Int, Item), Trade)])
itemList action clickedItem items = do
  let aggItems = M.elems $ Prelude.foldr (\i m -> case M.lookup (root i) m of
          Nothing -> M.insert (root i) i m
          Just i' -> M.insert (root i) i'{quantity = quantity i + quantity i'} m
        ) M.empty items
  simpleList (Prelude.filter (isValid . snd) <$> constDyn (Prelude.zip [0..] aggItems)) $ \itemDyn -> divClass "item" $ mdo
    (e, _) <- elAttr' "div" ("class" =: "item-info") $ do
      _ <- divClass "container-item" $ simpleList (fmap url . images . snd <$> itemDyn) imgWidget
      divClass "container-item" $ do
        el "h3" $ dynText $ name . snd <$> itemDyn
        el "p" $ dynText $ ("Quantity: " <>) . T.pack . printf "%.2f" . quantity . snd <$> itemDyn
        el "p" $ dynText $ maybe "" (("Quantity for sale: $" <>) . T.pack . show . saleQuantity) . sale . snd <$> itemDyn
        el "p" $ dynText $ maybe "" (("Price: $" <>) . T.pack . printf "%.2f" . price) . sale . snd <$> itemDyn
    let indexDyn = fst <$> itemDyn
        isItemClicked = maybe False (uncurry (==)) . (\(a,mb) -> (,) a <$> mb) <$> zipDyn indexDyn clickedItem
        tradeButton = fmap (const $ Left True) <$> buttonClass (T.toLower . T.pack $ show action) (T.pack $ show action)
    isTradeModalOpen <- holdDyn False $ either id (const True) <$> mTradeEv
    mTradeEvDyn  <- widgetHold tradeButton . updated $ isTradeModalOpen <&> \case
      True -> fmap (maybe (Left False) Right) <$> tradeWidget action (quantity . snd <$> itemDyn) isItemClicked
      False -> tradeButton
    let mTradeEv = switchDyn mTradeEvDyn
    let tradeEv = fmapMaybe id $ either (const Nothing) Just <$> mTradeEv
    setRoute $ (FrontendRoute_ItemDetail :/) <$> tagPromptlyDyn (root . snd <$> itemDyn) (domEvent Click e)
    pure $ attach (current itemDyn) tradeEv
  where imgWidget di = elDynAttr "img" ((\i -> "src" =: i <> "alt" =: i) <$> di) blank
        isValid i = name i /= "" && not (Prelude.null $ images i)

tradeWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     )
  => TradeAction -> Dynamic t Double -> Dynamic t Bool -> m (Event t (Maybe Trade))
tradeWidget action availableQuantity isItemClicked = divClass "trade-widget" $ mdo
  let tradeFinishedEv = fmapMaybe id $ bool Nothing (Just ()) <$> updated isItemClicked
      cancelEv = cancelClickEv -- leftmost [cancelClickEv, tradeFinishedEv]
  (qpDyn, cancelClickEv) <- divClass "trade-params-widget" $ do
    qpDyn' <- fmap join . widgetHold (marketParamsWidget availableQuantity) $ updated tradeTypeDyn <&> \case
      Market -> marketParamsWidget availableQuantity
      _ -> limitParamsWidget availableQuantity
    cancelClickEv' <- buttonClass "cancel" "X"
    pure (qpDyn', cancelClickEv')
  tradeTypeDyn <- tradeTypeWidget
  let tradeDyn = fmap (uncurry $ Trade action) <$> zipDynWith (fmap . (,)) tradeTypeDyn qpDyn
  tradeEv <- el "div" $ mdo
    clickedDyn <- holdDyn False $ leftmost [updated isItemClicked, True <$ tradeEv']
    clickEv <- dynButtonClass (T.toLower . T.pack $ show action) clickedDyn $ case action of
      Buy -> bool "Buy" "Buying..." <$> clickedDyn
      Sell -> bool "Sell" "Listing..." <$> clickedDyn
    let tradeEv' = fmapMaybe id $ tag (current tradeDyn) clickEv
    pure tradeEv'
  pure $ leftmost [Just <$> tradeEv, Nothing <$ cancelEv]

tradeTypeWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     )
  => m (Dynamic t TradeType)
tradeTypeWidget = mdo
  (marketClick, limitClick) <- divClass "trade-type-widget" $ do
    mClick <- selectableButtonClass "option" isMarketDyn "Market"
    lClick <- selectableButtonClass "option" (not <$> isMarketDyn) "Limit"
    pure (mClick, lClick)
  tradeTypeDyn <- holdDyn Market $ leftmost [Market <$ marketClick, Limit <$ limitClick]
  let isMarketDyn = (\case Market -> True; _ -> False) <$> tradeTypeDyn
  pure tradeTypeDyn

marketParamsWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     )
  => Dynamic t Double -> m (Dynamic t (Maybe QuantityAndPrice))
marketParamsWidget availableQuantity = el "form" $ do
  q <- quantityWidget availableQuantity
  pure $ fmap (flip QuantityAndPrice Nothing) <$> q

limitParamsWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     )
  => Dynamic t Double -> m (Dynamic t (Maybe QuantityAndPrice))
limitParamsWidget availableQuantity = el "form" $ do
  q <- quantityWidget availableQuantity
  p <- priceWidget
  pure $ fmap (\(q',p') -> QuantityAndPrice q' $ Just p') <$> zipDynWith (liftA2 (,)) q p

quantityWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     )
  => Dynamic t Double -> m (Dynamic t (Maybe Double))
quantityWidget availableQuantity = divClass "form-item" $ do
  el "div" $ text "Quantity"
  tQuantity <- inputElement $ def
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
      ("placeholder" =: "Quantity" <> "type" =: "text")
  let quantityDyn = T.unpack <$> value tQuantity
      isValid aq q = if q > aq then Left $ "That's too many! There are only " <> T.pack (printf "%0.2f" aq) <> " available"
                               else Right q
  let validQuantity = (\(aq, qd) -> maybe (Left $ if null qd then "" else "What? That's not a number") (isValid aq) $ readMaybe qd) <$> zipDyn availableQuantity quantityDyn
  divClass "error" $ dynText $ either id (const "") <$> validQuantity
  pure $ either (const Nothing) Just <$> validQuantity

priceWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     )
  => m (Dynamic t (Maybe Double))
priceWidget = divClass "form-item" $ do
  el "div" $ text "Price"
  tPrice <- inputElement $ def
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
      ("placeholder" =: "Price" <> "type" =: "text")
  let priceDyn = T.unpack <$> value tPrice
      validPrice = (\pd -> maybe (Left $ if null pd then "" else "What? That's not a number") Right $ readMaybe pd) <$> priceDyn
  divClass "error" $ dynText $ either id (const "") <$> validPrice
  pure $ either (const Nothing) Just <$> validPrice