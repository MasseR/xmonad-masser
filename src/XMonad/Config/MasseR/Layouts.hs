{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Config.MasseR.Layouts (layout) where

import XMonad (Window)

import XMonad.Layout (Full(..), Tall(..), (|||))
import XMonad.Layout.Accordion (Accordion(..))
import XMonad.Layout.Decoration (Decoration, DefaultShrinker)
import XMonad.Layout.DwmStyle (shrinkText)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Master (mastered)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import XMonad.Layout.Renamed (Rename(..), renamed)
import XMonad.Layout.Simplest (Simplest)
import XMonad.Layout.Tabbed (TabbedDecoration, Theme(..), tabbed)
import XMonad.Layout.ToggleLayouts (toggleLayouts)

import XMonad.Config.MasseR.Theme (defaultTheme, topBarTheme)
import XMonad.Layout.BinaryColumn (BinaryColumn(BinaryColumn))

-- Layout
myTabConfig ::  Theme
myTabConfig = defaultTheme


-- No signature because it's humongous
layout = smartBorders (toggleLayouts zoom workspaceLayouts)
  where
    bared = noFrillsDeco shrinkText topBarTheme
    zoom = renamed [Replace "Zoom"] (noBorders Full)
    workspaceLayouts = defLayout
    -- Default layout
    defLayout = accordionLayout ||| tabLayout ||| tiled ||| columnLayout
    accordionLayout = bared $ renamed [Replace "2/3"] (mastered (1/100) (2/3) Accordion)
    tiled = bared $ Tall nmaster delta ratio
    -- The binary column layout is useful when working with rotated screens,
    -- screens that are considerably taller than they are wide
    columnLayout = bared $ BinaryColumn 1.0 32
    -- I need to restrict the type or type inferencer can't deduce type classes
    tabLayout :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
    tabLayout = tabbed shrinkText myTabConfig
    delta = 3/100
    ratio = 1/2
    nmaster = 1
