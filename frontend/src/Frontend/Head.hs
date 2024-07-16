{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Head where

import Reflex.Dom.Core

import           Data.Char     (isSpace)
import           Data.Foldable (fold, traverse_)
import qualified Data.Map      as Map
import           Data.Text     (Text, pack)
import qualified Data.Text     as T

import Obelisk.Generated.Static

styleLink :: DomBuilder t m => Text -> m ()
styleLink href =
  elAttr "link" (Map.fromList [("href",href),("rel","stylesheet"),("type","text/css")]) blank

styleLink' :: DomBuilder t m => Text -> Text -> m ()
styleLink' href integrity =
  elAttr "link" (Map.fromList [("href",href),("rel","stylesheet"),("type","text/css"),("integrity",integrity),("crossorigin","anonymous")]) blank

scriptLink :: DomBuilder t m => Text -> m ()
scriptLink src = flip (elAttr "script") blank $ "src" =: src

scriptLink' :: DomBuilder t m => Text -> Text -> m ()
scriptLink' src integrity = flip (elAttr "script") blank $ Map.fromList
  [ ("src", src)
  , ("integrity", integrity)
  , ("crossorigin", "anonymous")
  ]

data FontType = Light | LightItalic | Regular | SemiBold | SemiBoldItalic | Bold | BoldItalic deriving stock (Eq, Ord, Enum, Show)

htmlHead :: (DomBuilder t m) => m ()
htmlHead = do
  el "title" $ text "Mercata Book"
  -- these are not the typesafe links so that the fonts load relatively to the css.
  elAttr "link" ("rel" =: "icon" <> "href" =: "static/favicon_large.png") $ blank
  elAttr "meta" ("name"=:"viewport" <> "content"=:"width=device-width, initial-scale=1, shrink-to-fit=no") $ blank
  elAttr "script" ("type" =: "text/javascript" <> "src" =: "static/bn.js") blank
  elAttr "script" ("type" =: "text/javascript" <> "src" =: "static/secp256k1.js") blank
  elAttr "script" ("type" =: "text/javascript" <> "src" =: "static/echarts.min.js") blank

  -- stylesheet links -------------------------------------------

  elAttr "style" ("type"=:"text/css") $ traverse_ (uncurry gfontsFontFamily)
    [ ("Merriweather Sans",[Regular, Bold])
    , ("Source Sans Pro",[Light .. BoldItalic])
    , ("Source Serif Pro",[Regular, Bold])
    , ("Titillium Web",[Regular, Bold])
    ]
  
  -- elAttr "script" ("type" =: "text/javascript" <> "src" =: $(static "echarts.min.js")) blank
  styleLink $(static "main.css")

gfontsFontFamily :: DomBuilder t m => Text -> [FontType] -> m ()
gfontsFontFamily ffName = traverse_ (gfontsFontFace ffName)

-- This would be better done with Clay
gfontsFontFace :: DomBuilder t m => Text -> FontType -> m ()
gfontsFontFace familyName fontType = traverse_ text
  [ "@font-face {"
  , "  font-family: '" <> T.toLower familyName <> "';"
  , "  font-style: " <> fontStyle <> ";"
  , "  font-weight: " <> fontWeight <> ";"
  , fold
    [ "  src: url(/static/gfonts/",snaked,"/",noSpaces,"-",styleName,".ttf)"
    , ";"
    ]
  , "} "
  ]
  where
    styleName = pack $ show fontType
    fontWeight = case fontType of
      Light          -> "300"
      LightItalic    -> "300"
      Regular        -> "400"
      SemiBold       -> "600"
      SemiBoldItalic -> "600"
      Bold           -> "700"
      BoldItalic     -> "700"
    fontStyle = case fontType of
      LightItalic    -> "italic"
      SemiBoldItalic -> "italic"
      BoldItalic     -> "italic"
      _              -> "normal"

    noSpaces = T.filter (not . isSpace) familyName
    snaked   = T.replace " " "_" familyName
