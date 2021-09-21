{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module XMonad.Config.MasseR  where


import Control.Lens


import qualified Data.List as List
import XMonad
import XMonad.Hooks.EwmhDesktops
       (ewmh, ewmhDesktopsStartup)
import XMonad.Hooks.ManageHelpers
       (doCenterFloat)
import XMonad.Hooks.SetWMName
       (setWMName)
import XMonad.Hooks.UrgencyHook
       (args, dzenUrgencyHook, withUrgencyHook)


import Data.Monoid
       (Endo)

import System.IO
       (hClose, hPutStr, stderr, hPutStrLn)
import XMonad.Actions.Navigation2D
import XMonad.Util.NamedActions
import XMonad.Util.Run
       (spawnPipe)

import qualified XMonad.Config.MasseR.ExtraConfig as C
import XMonad.Config.MasseR.Layouts
import XMonad.Config.MasseR.Theme
       (defaultTheme)
import XMonad.Layout.Decoration
       (Theme(..))



import System.Environment (setEnv)
import qualified XMonad.Config.MasseR.ExtraConfig as ExtraConfig
import qualified Dhall
import System.FilePath ((</>))
import Control.Exception (catch, SomeException)
import XMonad.Hooks.DynamicLog (statusBar)
import XMonad.XMobar (zenburnPP)
import XMonad.Prompt (XPConfig(font))
import XMonad.Config.MasseR.Bindings (keybindings)




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
  -- , extraHooks
  ]
  where
    classHook y = foldMap (\x -> className =? x --> y)
    titleHook y = foldMap (\x -> title =? x --> y)
    -- extraHooks :: Query (Endo WindowSet)
    -- extraHooks = foldMap (\r -> classHook (doShift (r ^. name . unpacked)) (r ^.. classes . traversed . unpacked)) rules
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


setPath :: String -> IO ()
setPath = setEnv "PATH"

masser :: IO ()
masser = do
  let xp = def{font="xft:Inconsolate-9"}
  defaults <- ExtraConfig.defaultConfig
  configFile <- fmap (</> "xmonad.dhall") getXMonadCacheDir
  -- Check journactl -xe for errors
  c <- ((defaults <>) <$> Dhall.inputFile Dhall.auto configFile) `catch` (\e -> defaults <$ hPutStrLn stderr (show @SomeException e))
  setPath (c ^. C.path)
  bar <- statusBar (c ^. C.applications . C.prompt) zenburnPP toggleStrutsKey (myConfig xp c)
  xmonad bar
  where
    toggleStrutsKey XConfig{modMask=modm} = (modm, xK_b)
    fullBindings xp c xc = keybindings (C.mkTopicConfig xp c) xp xc ^++^ C.mkBindings c xp xc
    myConfig xp c = withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"]} $
                     withNavigation2DConfig myNav2d $
                     ewmh $
                     addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) (fullBindings xp c) $
                     def {
                       modMask = mod4Mask -- Hyper
                       , terminal = c ^. C.applications . C.terminal
                       , keys = const mempty
                       , workspaces = C.mkWorkspaces c
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
                     }
