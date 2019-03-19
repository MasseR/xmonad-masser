{-# Language FlexibleContexts #-}
module Customizations (customizations) where

import Configurable
import XMonad
import XMonad.Util.EZConfig (additionalKeys)

import Customizations.Types
import qualified Customizations.Topics as Topics

import Control.Lens

customizations :: LayoutClass l Window => XConfig l -> XConfig l
customizations start = xconfig $ configure (mconcat [topics, Topics.customize, initial])
  where
    initial = EndoM $ \_ -> pure (Customize start def)
    topics :: LayoutClass l Window => Configurable (Customize l)
    topics = EndoM $ \super -> do
      _self <- ask
      pure (over _xconfig _ super)

