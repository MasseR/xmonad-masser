{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Config.MasseR.Layouts (layout) where

import XMonad
       (Window)

import XMonad.Layout
       (Full(..), Mirror(..), Tall(..), (|||))
import XMonad.Layout.Accordion
       (Accordion(..))
import XMonad.Layout.BinarySpacePartition
       (emptyBSP)
import XMonad.Layout.Decoration
       (Decoration, DefaultShrinker)
import XMonad.Layout.DwmStyle
       (dwmStyle, shrinkText)
import XMonad.Layout.LayoutModifier
       (ModifiedLayout)
import XMonad.Layout.Master
       (mastered)
import XMonad.Layout.NoBorders
       (noBorders, smartBorders)
import XMonad.Layout.NoFrillsDecoration
       (noFrillsDeco)
import XMonad.Layout.PerWorkspace
       (onWorkspace)
import XMonad.Layout.Renamed
       (Rename(..), renamed)
import XMonad.Layout.Simplest
       (Simplest)
import XMonad.Layout.Spiral
       (spiral)
import XMonad.Layout.Tabbed
       (TabbedDecoration, Theme(..), tabbed)
import XMonad.Layout.ToggleLayouts
       (toggleLayouts)

import XMonad.Config.MasseR.Theme
       (defaultTheme, topBarTheme)

-- Layout
myTabConfig ::  Theme
myTabConfig = defaultTheme


-- No signature because it's humongous
layout = noFrillsDeco shrinkText topBarTheme (smartBorders (toggleLayouts zoom workspaceLayouts))
  where
    zoom = renamed [Replace "Zoom"] (noBorders Full)
    workspaceLayouts = onWorkspace "web" webLayout $
                       onWorkspace "dynamics" webLayout $
                       onWorkspace "pdf" pdfLayout $
                       onWorkspace "documents" documentLayout $
                       onWorkspace "mail" mailLayout
                       defLayout
    -- Default layout
    defLayout = tiled ||| tabLayout ||| readLayout ||| bspLayout ||| vimLayout ||| spiral (6/7) ||| Full
    -- Pdfs are restricted to tabs
    vimLayout = Mirror (mastered (1/100) (4/5) Accordion)
    pdfLayout =  readLayout ||| tiled ||| tabLayout
    readLayout = renamed [Replace "2/3"] (dwmStyle shrinkText myTabConfig (mastered (1/100) (2/3) Accordion))
    bspLayout = renamed [Replace "master bsp"] (dwmStyle shrinkText myTabConfig (mastered (1/100) (2/3) (Mirror emptyBSP)))
    -- Documents are by default tabs, but have looser restrictions
    documentLayout = tabLayout ||| Full ||| tiled ||| Mirror tiled
    -- Web is either tabbed, full, or tiled
    webLayout = readLayout ||| tabLayout ||| Full ||| tiled
    tiled = Tall nmaster delta ratio
    -- I need to restrict the type or type inferencer can't deduce type classes
    tabLayout :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
    tabLayout = tabbed shrinkText myTabConfig
    mailLayout = readLayout ||| tabLayout
    delta = 3/100
    ratio = 1/2
    nmaster = 1
