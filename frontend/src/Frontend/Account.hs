{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.Account where

import Reflex.Dom.Core

import           Frontend.Login
import           Obelisk.Route
import           Obelisk.Route.Frontend   (SetRoute)
import           Common.Route

account
  :: forall t m
   . ( PostBuild t m
     , DomBuilder t m
     , Prerender t m
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
account = divClass "container" $ do
  logout
  pure ()