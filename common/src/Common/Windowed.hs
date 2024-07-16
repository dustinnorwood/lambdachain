{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Windowed
  ( Windowed(..)
  , limit
  , offset
  , item
  ) where

import Control.Lens
import Data.Text    (Text)

import Common.Api.Namespace  as Namespace (Namespace (Namespace))

data Windowed a = Windowed
  { _limit       :: Maybe Integer
  , _offset      :: Maybe Integer
  , _item        :: a
  } deriving (Eq, Ord, Show)
makeLenses ''Windowed