{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module XMonad.Config.MasseR  where


import qualified Data.List as List
import XMonad
import XMonad.Hooks.EwmhDesktops
       (ewmh, ewmhDesktopsStartup)
import XMonad.Hooks.SetWMName
       (setWMName)
import XMonad.Hooks.UrgencyHook
       (args, dzenUrgencyHook, withUrgencyHook)
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
       (emptyBSP)
import XMonad.Layout.Decoration
       (Decoration, DefaultShrinker)
import XMonad.Layout.DwmStyle
import XMonad.Layout.LayoutModifier
       (ModifiedLayout)
import XMonad.Layout.Master
import XMonad.Layout.NoBorders
       (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace
       (onWorkspace)
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
       (Simplest)
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
       (TabbedDecoration, tabbed)
import XMonad.Layout.ToggleLayouts
       (toggleLayouts)

import XMonad.XMobar
       (zenburnPP)

import Data.Monoid
       (Endo)

import XMonad.Util.SpawnOnce

import System.IO
       (hClose, hPutStr)
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdatePointer
       (updatePointer)
import XMonad.Util.NamedActions
import XMonad.Util.Run
       (spawnPipe)

import XMonad.Config.MasseR.Bindings
import XMonad.Config.MasseR.ExtraConfig


import qualified Data.Text as T

import XMonad.Hooks.DynamicLog
       (statusBar)

import qualified Data.Set as S




myNav2d :: Navigation2DConfig
myNav2d =
  def { defaultTiledNavigation = nav
      , screenNavigation = nav
      }
  where
    nav = hybridOf lineNavigation sideNavigation

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=terminus"
    hPutStr h (unlines $ showKm x)
    hClose h


-- Layout
myTabConfig ::  Theme
myTabConfig = def {
      activeBorderColor = "#DCDCCC"
    , activeTextColor = "#DCDCCC"
    , activeColor = "#3F3F3F"
    , fontName = "xft:Inconsolata-9"
    , inactiveBorderColor = "#262626"
    , inactiveTextColor = "#9FAFAF"
    , inactiveColor = "#262626"
  }


(=~?) :: XMonad.Query String -> String -> XMonad.Query Bool
q =~? x = fmap (x `List.isInfixOf`) q

-- Manage hooks
-- Move programs to their workspaces
myManageHook :: XMonad.Query (Endo WindowSet)
myManageHook = composeAll $ concat [
      dynamicsHook
    , webHooks
    , pdfHooks
    , documentHooks
    , floatHooks
    , debuggerHooks
    , flowHook
  ]
  where
    classHook y = map (\x -> className =? x --> y)
    webHooks = classHook (doShift "web") [
          "Firefox"
        , "qutebrowser"
        , "Midori"
        , "chromium-browser"
        , "Uzbl-tabbed"
        , "Uzbl-core"
      ]
    pdfHooks = classHook (doShift "pdf") [
          "Evince"
        , "Okular"
        , "Kpdf"
        , "Xdvi"
        , ".zathura-wrapped_"
      ]
    documentHooks = classHook (doShift "documents") [
          "libreoffice"
        , "libreoffice-calc"
        , "Assistant"
        , "Bouml" -- Oh wow, didn't even remember this existed
      ]
    floatHooks = classHook doFloat [
          "SMplayer"
        , "Gimp"
        , "MPlayer"
        , "Kaffeine"
        , "Xmessage"
        , "Wfica_Seamless" -- I think this is citrix
        , "mpv"
      ]
    debuggerHooks = classHook (doShift "debugger") [
          "JSwat Debugger", -- Haven't used this in years. A good thing?
          "DBeaver"
      ]
    dynamicsHook = [title =~? "Dynamics" --> doShift "dynamics"]
    flowHook = [title =~? "www.flowdock.com" --> doShift "flowdock"]




myStartupHook :: X ()
myStartupHook = spawnOnce "$HOME/wminit"

masser :: ExtraConfig -> IO ()
masser extraConfig = xmonad =<< statusBar (bar extraConfig) zenburnPP toggleStrutsKey myConfig
  where
    toggleStrutsKey XConfig{modMask=modm} = (modm, xK_b)
    bar = prompt . applications
    myConfig = withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"]} $
                     withNavigation2DConfig myNav2d $
                     ewmh $
                     addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) (keybindings extraConfig) $
                     def {
                       modMask = mod4Mask -- Hyper
                       , terminal = urxvt . applications $ extraConfig
                       , keys = const mempty
                       , workspaces = let defaults = ["irc", "web", "mail"]
                                          external = map (T.unpack . topicName) . topics $ extraConfig
                                      in S.toList (S.fromList defaults <> S.fromList external)
                       , layoutHook = smartBorders myLayout
                       , clickJustFocuses = False
                       , startupHook = myStartupHook >> ewmhDesktopsStartup >> setWMName "LG3D"
                       , borderWidth = 2
                       , normalBorderColor = "#262626"
                       , focusedBorderColor = "#7F9F7F"
                       , manageHook = myManageHook
                       -- The focus follows mouse is a bad idea for me because
                       -- it misbehaves with accordion. If I accidentally hover
                       -- my mouse at the lower edge of the accordion, it will
                       -- just cycle through to the last accordion
                       , focusFollowsMouse = False
                       , logHook = updatePointer (0.25, 0.25) (0.25, 0.25)
                     }
    myLayout = toggleLayouts zoom workspaceLayouts
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
