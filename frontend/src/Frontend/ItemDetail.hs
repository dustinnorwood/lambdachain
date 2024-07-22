{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.ItemDetail where


import           Blockchain.Data.Address
import           Blockchain.Data.Keys
import           Blockchain.Data.RLP
import           Common.Route
import           Common.Utils
import           Control.Lens
import           Control.Monad          (void, (<=<), when)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Bifunctor         (first)
import           Data.Bool              (bool)
import           Data.List              (sortOn)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromJust, fromMaybe, isJust, listToMaybe)
import           Data.Ord               (Down(..))
import qualified Data.Text              as T
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8)
import           ECharts.ChartOptions
import           Frontend.Client        (urlGET, urlGETDyn)
import           Frontend.ItemList
import           Frontend.Utils         (dynButton)
import           GHC.Float              (int2Double)
import           Language.Javascript.JSaddle
import           Mercata.Data.Item
import           Obelisk.Frontend.Storage
import           Obelisk.Route.Frontend   (R, Routed, SetRoute, askRoute)
import           Reflex.Dom.Core
import           GHCJS.DOM.Document       (createElement)
import           GHCJS.DOM.Element        (setInnerHTML)
import           Reflex.Dom.Widget.ECharts
import           Text.Printf
import GHC.Generics (DecidedStrictness(DecidedStrict))

itemDetail
  :: forall t m
  . ( PostBuild t m
     , PerformEvent t m
     , DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , Prerender t m
     , MonadIO (Performable m)
     , SetRoute t (R FrontendRoute) m
     , TriggerEvent t m
     , Routed t Address m
     )
  => Dynamic t (Maybe (Text, PrivateKey)) -> m ()
itemDetail mCreds = divClass "container" $ mdo
  pBE <- getPostBuild
  rootAddrDyn <- askRoute
  username2 <- fmap (fmap snd) . getStorageItem $ "mercata_username" <$ pBE
  let mUsernameE = leftmost [fmap fst <$> updated mCreds, username2]
  let username = fmapMaybe id mUsernameE
  usernameDyn <- holdDyn Nothing mUsernameE
  let assetUrlDyn = ("https://lambdachain.xyz/cirrus/search/BlockApps-Mercata-Asset?limit=100&order=root&or=(data->>isMint.eq.true,quantity.gt.0)&select=*,BlockApps-Mercata-Sale!BlockApps-Mercata-Sale_BlockApps-Mercata-Asset_fk(*,BlockApps-Mercata-Sale-paymentProviders(*)),BlockApps-Mercata-Asset-images(*),BlockApps-Mercata-Bid(*)&root=eq." <>) . tshow <$> rootAddrDyn
  itemEv <- fmap (fromMaybe emptyItem . listToMaybe . aggregateItems . fromMaybe []) <$> urlGETDyn assetUrlDyn
  itemDyn <- holdDyn emptyItem itemEv

  let addZero delta ((p,q):xs) = (delta p,0):(p,q):xs
      addZero _ [] = []
  let cumulativeBids = tail . scanl (\(_,q) i -> (bidPrice i, q + bidQuantity i)) (0,0) . sortOn (Down . bidPrice) . M.elems . bids . bidded
  let cumulativeBidsDyn = cumulativeBids <$> itemDyn

  let cumulativeAsks = tail . scanl (\(_,q) i -> (salePrice i, q + saleQuantity i)) (0,0) . sortOn salePrice . map (fromJust . sale) . M.elems . utxos . forSale
  let cumulativeAsksDyn = cumulativeAsks <$> itemDyn

  divClass "item" . divClass "item-container" $ do
    divClass "item-container" $ itemInfoWidget itemDyn
    el "br" blank
    el "h3" $ text "Description"
    prerender_ (text "Loading Description...") $ do
      d <- askDocument
      itemT <- sample $ current itemDyn
      e <- liftJSM $ do
        e <- createElement d ("div" :: String)
        setInnerHTML e . T.unpack $ description itemT
        pure e
      performEvent_ $ (liftJSM . setInnerHTML e . description) <$> updated itemDyn
      placeRawElement e
    pure ()
  divClass "item-container" $ do
    prerender_ (pure ()) $ do
      pb <- getPostBuild
      let fillInBid' :: Num a => [(Double, a)] -> [(Integer, a)]
          fillInBid' ((k1,v1):(k2,v2):ks) = zip [floor (100 * k1) + 1 .. (floor $ 100 * k2)] (repeat v2) ++ fillInBid' ((k2,v2):ks)
          fillInBid' [(k1,v1)] = [(floor (100 * k1) + 1, 0)]
          fillInBid' [] = []
          fillInBid :: Num a => [(Double, a)] -> [(Integer, a)]
          fillInBid ((k1,v1):ks) = zip [0 .. floor (100 * k1)] (repeat v1) ++ fillInBid' ((k1,v1):ks)
          fillInBid [] = []
      let fillInAsk' :: [(Double, a)] -> [(Integer, a)]
          fillInAsk' ((k1,v1):(k2,v2):ks) = zip [floor (100 * k1) .. (floor $ 100 * k2)-1] (repeat v1) ++ fillInAsk' ((k2,v2):ks)
          fillInAsk' ((k1,v1):ks) = zip [floor (100 * k1) .. floor (100 * (k1 + 1))] (repeat v1) ++ fillInAsk' ks
          fillInAsk' [] = []
          fillInAsk :: [(Double, a)] -> [(Integer, a)]
          fillInAsk ((k1,v1):ks) = zip [(100 * floor k1) .. (floor (100 * k1) - 1)] (repeat v1) ++ fillInAsk' ((k1,v1):ks)
          fillInAsk [] = []
      _ <- elAttr "div" ("style" =: "padding: 0px;") $ bidAskSpreadChart (fillInBid . reverse <$> cumulativeBidsDyn) (fillInAsk <$> cumulativeAsksDyn)
      pure ()
  el "br" blank
  el "br" blank
  pure ()
  where imgWidget di = elDynAttr "img" ((\i -> "src" =: i <> "alt" =: i) <$> di) blank
        isValid i = name i /= "" && not (Prelude.null $ images i)

bidAskSpreadChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => Dynamic t [(Integer, Integer)] -> Dynamic t [(Integer, Integer)] -> m (Chart t)
bidAskSpreadChart bidDyn askDyn = do
  let fmt i = T.pack (show $ i `div` 100) <> "." <> T.pack (printf "%02d" $ i `mod` 100)
  let totalXData = map fmt . (\case [] -> []; (x:xs) -> [x .. last xs]) . map fst <$> zipDynWith (++) bidDyn askDyn
  let seriesData d = map (\(k,v) -> (fmt k, DataInt $ fromInteger v)) <$> d
  let xAxisData d = map fst <$> d
      mkSeries i z s = let sd = seriesData s
                        in (i :: Int) =: (z, M.fromList <$> sd, xAxisData sd)
  let chartDataDyn = (mkSeries 0 bidSeries bidDyn) <> (mkSeries 1 askSeries askDyn)
      ddSeries :: Reflex.Dom.Widget.ECharts.Series SeriesLine
      ddSeries = def
        & series_step ?~ Step_Start
        & series_showSymbol ?~ False
      bidSeries = ddSeries
        & series_lineStyle ?~ (def
              & lineStyle_color ?~ "#20c020"
            )
        & series_areaStyle ?~ (def
              & areaStyle_color ?~ "#20c020"
              & areaStyle_opacity ?~ 0.7
            )
      askSeries = ddSeries
        & series_lineStyle ?~ (def
              & lineStyle_color ?~ "#c02020"
            )
        & series_areaStyle ?~ (def
              & areaStyle_color ?~ "#c02020"
              & areaStyle_opacity ?~ 0.7
            )

  lineChart (LineChartConfig (300, 200)
              (basicLineChartOpts <$> totalXData)
              chartDataDyn
            )
  where
    basicLineChartOpts :: [Text] -> ChartOptions
    basicLineChartOpts xData = def
      & chartOptions_title ?~ (def & title_text ?~ "Bid/Ask Spread")
      & chartOptions_yAxis .~ (def
        & axis_type ?~ AxisType_Value
        ) : []
      & chartOptions_xAxis .~ (def
        & axis_type ?~ AxisType_Category
        & axis_data ?~ (Prelude.zip xData $ repeat Nothing)
        ) : []