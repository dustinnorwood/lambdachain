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
import           Common.Utils
import           Control.Applicative    (liftA2, (<|>))
import           Control.Monad          (join, void)
import           Control.Monad.Fix      (MonadFix)
import           Data.Bool              (bool)
import           Data.Functor           ((<&>))
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromJust)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Frontend.Utils
import           Mercata.Data.Item
import           Mercata.Data.Trade
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (SetRoute, setRoute)
import           Reflex.Dom.Core        hiding (Delete)
import           Text.Read              (readMaybe)

itemList
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , SetRoute t (R FrontendRoute) m
     )
  => TradeAction -> Text -> (Event t Trade -> m (Async t Text)) -> [Item] -> m (Event t ())
itemList action username executeTrade items = switchDyn . fmap leftmost <$> do
  simpleList (Prelude.filter isValid <$> constDyn items) $ \itemDyn -> divClass "item" $ mdo
    itemDetailClick <- itemInfoWidget itemDyn
    tradeEv <- tradeButtons action username itemDyn executeTrade
    setRoute $ (FrontendRoute_ItemDetail :/) <$> tagPromptlyDyn (root <$> itemDyn) itemDetailClick
    pure . fmapMaybe id $ bool Nothing (Just ()) <$> tradeEv
  where isValid i = name i /= "" && not (Prelude.null $ images i) && quantity i > 0

tradeButtons
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     )
  => TradeAction -> Text -> Dynamic t Item -> (Event t Trade -> m (Async t Text)) -> m (Event t Bool)
tradeButtons action username itemDyn executeTrade = do
  pBE <- getPostBuild
  case action of
    Buy -> fmap switchDyn . widgetHold createTradeButton $
      tag (current itemDyn) pBE <&> \item -> case M.elems . bids $ biddedBy username item of
        [] -> fmap (fmap $ either (const False) id) . toggleWidget createTradeButton $ tradeWidget action itemDyn executeTrade
        bs -> do
          b <- mdo
            --let buttons = do
            isToggled <- holdDyn Nothing $ fst <$> toggleE
            let closed = divClass "trade-type-widget" $ do
                  newClick <- optionCreateTradeButton
                  updateClick <- optionUpdateTradeButton
                  pure . fmap ((,False) . Just) $ leftmost [False <$ newClick, True <$ updateClick]
            toggleD <- widgetHold closed . updated $ isToggled <&> \case
              Just False -> fmap (Nothing,) <$> tradeWidget action itemDyn executeTrade
              Just True -> fmap (Nothing,) <$> updateTradeWidget action username itemDyn executeTrade
              Nothing -> closed
            let toggleE = switchDyn toggleD
            pure toggleE
          pure $ snd <$> b
    Sell -> fmap switchDyn . widgetHold createTradeButton $
      tag (current itemDyn) pBE <&> \item -> case M.elems . utxos $ forSaleBy username item of
        [] -> fmap (fmap $ either (const False) id) . toggleWidget createTradeButton $ tradeWidget action itemDyn executeTrade
        bs -> do
          b <- mdo
            --let buttons = do
            isToggled <- holdDyn Nothing $ fst <$> toggleE
            let closed = do -- divClass "trade-type-widget" $ do
                  -- newClick <- optionCreateTradeButton
                  updateClick <- updateTradeButton
                  pure . fmap ((,False) . Just) $ True <$ updateClick -- leftmost [False <$ newClick, True <$ updateClick]
            toggleD <- widgetHold closed . updated $ isToggled <&> \case
              Just False -> fmap (Nothing,) <$> tradeWidget action itemDyn executeTrade
              Just True -> fmap (Nothing,) <$> updateTradeWidget action username itemDyn executeTrade
              Nothing -> closed
            let toggleE = switchDyn toggleD
            pure toggleE
          pure $ snd <$> b
  where createTradeButton = fmap (const False) <$> buttonClass (T.toLower $ tshow action) (tshow action)
        updateTradeButton = fmap (const False) <$> buttonClass (T.toLower $ tshow action) ("Update " <> tradeName action)
        optionCreateTradeButton = buttonClass (T.append "option-" . T.toLower $ tshow action) (tshow action)
        optionUpdateTradeButton = buttonClass (T.append "option-" . T.toLower $ tshow action) ("Update " <> tradeName action)

itemInfoWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t Item -> m (Event t ())
itemInfoWidget itemDyn = do
  (e,_) <- elAttr' "div" ("class" =: "item-info") $ do
    _ <- divClass "container-item" $ simpleList (fmap url . images <$> itemDyn) imgWidget
    divClass "container-item" $ do
      el "h3" $ dynText $ name <$> itemDyn
      el "div" $ dynText $ ("Circulating supply: " <>) . tshow . quantity <$> itemDyn
      el "div" $ dynText $ itemDyn <&> \item -> case bidPriceRange item of
        Nothing -> ""
        Just (Left p) -> "Bid price: " <> printPriceUSD p
        Just (Right (l,h)) -> "Bid price range: " <> printPriceUSD l <> " - " <> printPriceUSD h
      el "div" $ dynText $ itemDyn <&> \item -> case salePriceRange item of
        Nothing -> ""
        Just (Left p) -> "Ask price: " <> printPriceUSD p
        Just (Right (l,h)) -> "Ask price range: " <> printPriceUSD l <> " - " <> printPriceUSD h
  pure $ domEvent Click e
  where imgWidget di = elDynAttr "img" ((\i -> "src" =: i <> "alt" =: i) <$> di) blank

tradeWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     )
  => TradeAction -> Dynamic t Item -> (Event t Trade -> m (Async t Text)) -> m (Event t Bool)
tradeWidget action itemDyn executeTrade = divClass "trade-widget" $ mdo
  (qpDyn, cancelled) <- divClass "trade-params-widget-container" $ do
    qpDyn' <- tradeParamsWidget Nothing itemDyn tradeTypeDyn
    (qpDyn',) <$> buttonClass "cancel" "X"
  tradeTypeDyn <- divClass "trade-type-widget" tradeTypeWidget
  let tqpDyn = zipDyn tradeTypeDyn qpDyn
      tqpiDyn = zipDyn tqpDyn itemDyn
      tradeDyn = (\((t,mqp),i) -> Trade Create t action tokenPsAddr i <$> mqp) <$> tqpiDyn
  clickedDyn <- holdDyn False $ leftmost [True <$ started transaction, False <$ finished transaction]
  clickEv <- dynButtonClass (T.toLower $ tshow action) clickedDyn $ case action of
    Buy -> bool "Buy" "Buying..." <$> clickedDyn
    Sell -> bool "Sell" "Listing..." <$> clickedDyn
  let tradeEv' = fmapMaybe id $ tag (current tradeDyn) clickEv
  transaction <- executeTrade tradeEv'
  pure $ leftmost [True <$ finished transaction, False <$ cancelled]

updateTradeWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     )
  => TradeAction -> Text -> Dynamic t Item -> (Event t Trade -> m (Async t Text)) -> m (Event t Bool)
updateTradeWidget action username itemDyn executeTrade = divClass "trade-widget" $ do
  cancelled <- divClass "cancel-button" $ (False <$) <$> buttonClass "cancel" "X"
  reload <- case action of
    Buy -> switchDyn . fmap leftmost <$> do
      simpleList (M.elems . bids . biddedBy username <$> itemDyn) $ \bidDyn -> divClass "item" $ mdo
        qpDyn <- divClass "trade-params-widget-container" $ do
          pBE <- getPostBuild
          fmap join . widgetHold (pure $ constDyn Nothing) $ tagPromptlyDyn bidDyn pBE <&> \bid ->
            tradeParamsWidget (Just $ QuantityAndPrice (bidQuantity bid) (Just $ bidPrice bid)) itemDyn (constDyn Limit)
        let qpiDyn = zipDyn qpDyn itemDyn

        transactionFinished <- divClass "button-options" $ do
          updateTransaction <- mdo
            let tradeDyn = (\(b, (mqp,i)) -> Trade Update Limit action tokenPsAddr i{bids = bidAddress b =: b} <$> mqp) <$> zipDyn bidDyn qpiDyn
            clickedDyn <- holdDyn False $ leftmost [True <$ started transaction, False <$ finished transaction]
            clickEv <- dynButtonClass (T.append "option-" . T.toLower $ tshow action) clickedDyn $
              bool "Update Bid" "Updating Bid..." <$> clickedDyn
            let tradeEv' = fmapMaybe id $ tag (current tradeDyn) clickEv
            transaction <- executeTrade tradeEv'
            pure transaction
          deleteTransaction <- mdo
            let tradeDyn = (\(b, (mqp,i)) -> Trade Delete Limit action tokenPsAddr i{bids = bidAddress b =: b} <$> mqp) <$> zipDyn bidDyn qpiDyn
            clickedDyn <- holdDyn False $ leftmost [True <$ started transaction, False <$ finished transaction]
            clickEv <- dynButtonClass (T.append "option-" . T.toLower $ tshow action) clickedDyn $
              bool "Delete Bid" "Deleting Bid..." <$> clickedDyn
            let tradeEv' = fmapMaybe id $ tag (current tradeDyn) clickEv
            transaction <- executeTrade tradeEv'
            pure transaction
          pure $ leftmost [finished updateTransaction, finished deleteTransaction]
        pure $ True <$ transactionFinished
    Sell -> switchDyn . fmap leftmost <$> do
      simpleList (M.elems . utxos . forSaleBy username <$> itemDyn) $ \utxoDyn -> divClass "item" $ mdo
        let askDyn = fromJust . sale <$> utxoDyn
        qpDyn <- divClass "trade-params-widget-container" $ do
          pBE <- getPostBuild
          fmap join . widgetHold (pure $ constDyn Nothing) $ tagPromptlyDyn askDyn pBE <&> \ask ->
            tradeParamsWidget (Just $ QuantityAndPrice (saleQuantity ask) (Just $ salePrice ask)) itemDyn (constDyn Limit)
        let qpiDyn = zipDyn qpDyn itemDyn

        transactionFinished <- divClass "button-options" $ do
          updateTransaction <- mdo
            let tradeDyn = (\(u, (mqp,i)) -> Trade Update Limit action tokenPsAddr i{utxos = address u =: u} <$> mqp) <$> zipDyn utxoDyn qpiDyn
            clickedDyn <- holdDyn False $ leftmost [True <$ started transaction, False <$ finished transaction]
            clickEv <- dynButtonClass (T.append "option-" . T.toLower $ tshow action) clickedDyn $
              bool "Update Ask" "Updating Ask..." <$> clickedDyn
            let tradeEv' = fmapMaybe id $ tag (current tradeDyn) clickEv
            transaction <- executeTrade tradeEv'
            pure transaction
          deleteTransaction <- mdo
            let tradeDyn = (\(u, (mqp,i)) -> Trade Delete Limit action tokenPsAddr i{utxos = address u =: u} <$> mqp) <$> zipDyn utxoDyn qpiDyn
            clickedDyn <- holdDyn False $ leftmost [True <$ started transaction, False <$ finished transaction]
            clickEv <- dynButtonClass (T.append "option-" . T.toLower $ tshow action) clickedDyn $
              bool "Delete Ask" "Deleting Ask..." <$> clickedDyn
            let tradeEv' = fmapMaybe id $ tag (current tradeDyn) clickEv
            transaction <- executeTrade tradeEv'
            pure transaction
          pure $ leftmost [finished updateTransaction, finished deleteTransaction]
        pure $ True <$ transactionFinished
  pure $ leftmost [reload, cancelled]

tradeTypeWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     )
  => m (Dynamic t TradeType)
tradeTypeWidget = mdo
  (marketClick, limitClick) <- do
    mClick <- selectableButtonClass "option" isMarketDyn "Market"
    lClick <- selectableButtonClass "option" (not <$> isMarketDyn) "Limit"
    pure (mClick, lClick)
  tradeTypeDyn <- foldDyn const Market (leftmost [Market <$ marketClick, Limit <$ limitClick])
  let isMarketDyn = (\case Market -> True; _ -> False) <$> tradeTypeDyn
  pure tradeTypeDyn

tradeParamsWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     , MonadHold t m
     )
  => Maybe QuantityAndPrice -> Dynamic t Item -> Dynamic t TradeType -> m (Dynamic t (Maybe QuantityAndPrice))
tradeParamsWidget mQP itemDyn tradeTypeDyn = divClass "trade-params-widget" . el "form" $ do
  pBE <- getPostBuild
  q <- quantityWidget (_qp_quantity <$> mQP) $ quantity <$> itemDyn
  let emptyField = el "form-item" $ constDyn Nothing <$ el "br" blank
  let tt = leftmost [updated tradeTypeDyn, tagPromptlyDyn tradeTypeDyn pBE]
  p <- fmap join . widgetHold emptyField $ tt <&> \case
    Market -> emptyField
    _ -> fmap Just <$> priceWidget (_qp_price =<< mQP)
  let qpDyn = zipDynWith (\q' p' -> case p' of 
        Nothing -> flip QuantityAndPrice Nothing <$> q'
        Just p'' -> uncurry (flip QuantityAndPrice . Just) <$> liftA2 (,) p'' q'
        ) q p
  pure qpDyn

quantityWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     )
  => Maybe Integer -> Dynamic t Integer -> m (Dynamic t (Maybe Integer))
quantityWidget mInitialValue availableQuantity = divClass "form-item" $ do
  tQuantity <- divClass "form-item-item" $ do
    divClass "form-item-item-text-wrapper" $ divClass "form-item-item-text" $ el "div" $ text "Quantity"
    divClass "form-item-item-entry" $ inputElement $ def
      & inputElementConfig_initialValue .~ maybe "" tshow mInitialValue
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Quantity" <> "type" =: "text")
  let quantityDyn = T.unpack <$> value tQuantity
      isValid aq q = if q > aq then Left $ "That's too many! There are only " <> tshow aq <> " available"
                               else Right q
      readNonEmpty aq qd = if null qd
                             then Left ""
                             else maybe (Left "What? That's not a number") (isValid aq) $ readMaybe qd <|> readMaybe (qd <> "0")
  let validQuantity = uncurry readNonEmpty <$> zipDyn availableQuantity quantityDyn
  divClass "error" $ dynText $ either id (const "") <$> validQuantity
  pure $ either (const Nothing) Just <$> validQuantity

priceWidget
  :: forall t m
  . ( PostBuild t m
     , DomBuilder t m
     )
  => Maybe Double -> m (Dynamic t (Maybe Double))
priceWidget mInitialValue = divClass "form-item" $ do
  tPrice <- divClass "form-item-item" $ do
    divClass "form-item-item-text-wrapper" $ divClass "form-item-item-text" $ text "Price"
    divClass "form-item-item-entry" $ inputElement $ def
      & inputElementConfig_initialValue .~ maybe "" printPrice mInitialValue
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Price" <> "type" =: "text")
  let priceDyn = T.unpack <$> value tPrice
      validPrice = (\pd -> maybe (Left $ if null pd then "" else "What? That's not a number") Right $ readMaybe pd <|> readMaybe (pd <> "0")) <$> priceDyn
  divClass "error" $ dynText $ either id (const "") <$> validPrice
  pure $ either (const Nothing) Just <$> validPrice