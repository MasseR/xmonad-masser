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
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.UrgencyHook
       (args, dzenUrgencyHook, withUrgencyHook)

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
import XMonad.Config.MasseR.Layouts
import XMonad.Config.MasseR.ExtraConfig
import XMonad.Config.MasseR.Theme (defaultTheme)
import XMonad.Layout.Decoration
       (Theme(..))


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

(=~?) :: XMonad.Query String -> String -> XMonad.Query Bool
q =~? x = fmap (x `List.isInfixOf`) q

-- Manage hooks
-- Move programs to their workspaces
myManageHook :: XMonad.Query (Endo WindowSet)
myManageHook = mconcat
  [ webHooks
  , pdfHooks
  , documentHooks
  , floatHooks
  , debuggerHooks
  , flowHook
  , pipHooks
  ]
  where
    classHook y = foldMap (\x -> className =? x --> y)
    titleHook y = foldMap (\x -> title =? x --> y)
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
      ]
    floatHooks = classHook doFloat [
          "Gimp"
        , "Xmessage"
        , "mpv"
      ]
    -- The new picture in picture mode in firefox
    pipHooks = titleHook doCenterFloat [ "Picture-in-Picture" ]
    debuggerHooks = classHook (doShift "debugger") [
          "DBeaver"
      ]
    flowHook = titleHook (doShift "flowdock") ["www.flowdock.com"]




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
                       , layoutHook = layout
                       , clickJustFocuses = False
                       , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
                       , borderWidth = 2
                       , normalBorderColor = inactiveBorderColor defaultTheme
                       , focusedBorderColor = activeBorderColor defaultTheme
                       , manageHook = myManageHook
                       -- The focus follows mouse is a bad idea for me because
                       -- it misbehaves with accordion. If I accidentally hover
                       -- my mouse at the lower edge of the accordion, it will
                       -- just cycle through to the last accordion
                       , focusFollowsMouse = False
                       , logHook = updatePointer (0.25, 0.25) (0.25, 0.25)
                     }
