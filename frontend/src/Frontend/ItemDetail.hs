{-# LANGUAGE FlexibleContexts #-}
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
import           Control.Applicative    (liftA2)
import           Control.Lens
import           Control.Monad          (void, (<=<), when)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Bifunctor         (first)
import           Data.Bool              (bool)
import           Data.List              (sortOn)
import qualified Data.Map.Strict        as M
import           Data.Maybe             (fromMaybe, isJust, listToMaybe)
import qualified Data.Text              as T
import           Data.Text              (Text)
import           Data.Text.Encoding     (decodeUtf8)
import           ECharts.ChartOptions
import           Frontend.Client        (urlGET, urlGETDyn)
import           Frontend.Utils         (dynButton)
import           Language.Javascript.JSaddle
import           Mercata.Data.Item
import           Obelisk.Frontend.Storage
import           Obelisk.Route.Frontend   (R, Routed, SetRoute, askRoute)
import           Reflex.Dom.Core
import           Reflex.Dom.Widget.ECharts
import           Text.Printf

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
  let assetUrlDyn = ("https://lambdachain.xyz/cirrus/search/BlockApps-Mercata-Asset?select=*,BlockApps-Mercata-Sale!BlockApps-Mercata-Sale_BlockApps-Mercata-Asset_fk(*),BlockApps-Mercata-Asset-images(*)&root=eq." <>) . T.pack . show <$> rootAddrDyn
  (itemsEv :: Event t [Item]) <- fmap (fromMaybe []) <$> urlGETDyn assetUrlDyn
  let aggAllItems = M.elems . Prelude.foldr (\i m -> case M.lookup (root i) m of
          Nothing -> M.insert (root i) i m
          Just i' -> M.insert (root i) i'{quantity = quantity i + quantity i'} m
        ) M.empty
  let aggOwnedItems = Prelude.foldr (\i m -> case M.lookup (ownerCommonName i) m of
          Nothing -> M.insert (ownerCommonName i) i m
          Just i' -> M.insert (ownerCommonName i) i'{quantity = quantity i + quantity i'} m
        ) M.empty
  let forSaleItems = mapMaybe sale
  let aggForSaleItems = sum . Prelude.map saleQuantity . forSaleItems
  let cumulativeAsks = scanl (\(_,q) i -> (price i, q + saleQuantity i)) (0,0) . sortOn price . forSaleItems
  itemsDyn <- holdDyn [] itemsEv
  let mItemsDyn = listToMaybe . aggAllItems <$> itemsDyn
  let mOwnedItemsDyn = ((\(u,i) -> M.lookup u $ aggOwnedItems i) =<<) <$> zipDynWith (\mu i -> (,i) <$> mu) usernameDyn itemsDyn
  let quantityForSaleDyn = aggForSaleItems <$> itemsDyn
  let cumulativeAsksDyn = cumulativeAsks <$> itemsDyn
  -- cumulativeAsksDyn <- holdDyn [(0.0,0.0),(1.0,1.0),(2.0,4.0),(3.0,9.0),(4.0,16.0)] $ cumulativeAsks <$> itemsEv
  divClass "item" . divClass "item-container" $ do
    divClass "item-container" $ do
      divClass "container-item" $ simpleList (maybe [] (fmap url . images) <$> mItemsDyn) imgWidget
      divClass "container-item" $ do
        el "h3" $ dynText $ maybe "" name <$> mItemsDyn
        el "p" $ dynText $ maybe "" (("Quantity: " <>) . T.pack . printf "%.2f" . quantity) <$> mItemsDyn
        el "p" $ dynText $ maybe "" (("Quantity Owned: " <>) . T.pack . printf "%.2f" . quantity) <$> mOwnedItemsDyn
        el "p" $ dynText $ ("Quantity For Sale: " <>) . T.pack . printf "%.2f" <$> quantityForSaleDyn
        el "p" $ dynText $ maybe "" (maybe "" (("Price: $" <>) . T.pack . printf "%.2f" . price) . sale) <$> mItemsDyn
      pure ()
  divClass "item-container" $ do
    prerender_ (pure ()) $ do
      pb <- getPostBuild
      let zeroHead [] = [] 
          zeroHead ((k,v):kvs) = (k-1.0,0.0):(k,v):kvs
      let extendLast [] = []
          extendLast [(k,v)] = [(k,v),(k+10.0,v)]
          extendLast (x:xs) = x:extendLast xs
          fillIn ((k1,v1):(k2,v2):ks) = zip [k1, (k1+0.01) .. k2] (repeat v1) ++ fillIn ((k2,v2):ks)
          fillIn ((k1,v1):ks) = zip [k1, (k1+0.01) .. (k1+10.0)] (repeat v1) ++ fillIn ks
          fillIn ks = ks
      -- _ <- elAttr "div" ("style" =: "padding: 0px;") $ basicLineChart $ zeroHead . extendLast . tail <$> cumulativeAsksDyn
      _ <- elAttr "div" ("style" =: "padding: 0px;") $ basicLineChart $ fillIn <$> cumulativeAsksDyn
      -- _ <- elAttr "div" ("style" =: "padding: 0px;") $ basicLineChart2
      pure ()
  el "br" blank
  el "br" blank
  pure ()
  where imgWidget di = elDynAttr "img" ((\i -> "src" =: i <> "alt" =: i) <$> di) blank
        isValid i = name i /= "" && not (Prelude.null $ images i)

basicLineChart
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
  => Dynamic t [(Double, Double)] -> m (Chart t)
basicLineChart dataDyn = do
  let fmt = T.pack . printf "%0.2f"
  let xAxisData = map (fmt . fst) <$> dataDyn
  let seriesData = M.fromList . map (\(k,v) -> (fmt k, DataDouble v)) <$> dataDyn
  let chartDataDyn = ((0 :: Int) =: (ddSeries, seriesData, xAxisData)) -- <> (1 =: (dd2Series, dd2, xd))
      ddSeries = def
        -- & series_smooth ?~ Left True
        & series_step ?~ Step_Start
        & series_showSymbol ?~ False
        & series_areaStyle ?~ (def
              & areaStyle_color ?~ "#c02020"
              & areaStyle_opacity ?~ 0.7
            )

  lineChart (LineChartConfig (300, 200)
              (basicLineChartOpts <$> xAxisData)
              chartDataDyn
            )
  where
    -- yAxisData = M.fromList $ zip xAxisData $ map DataInt $ reverse dataVals
    -- yAxisData2 = M.fromList $ zip xAxisData $ map DataInt dataVals
    dataVals :: [Int]
    dataVals = [820, 932, 901, 934, 1290, 1330, 1320]
    -- xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
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