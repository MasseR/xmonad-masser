module XMonad.CustomPrompt (xdgOpen, browser) where

import qualified XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt
import XMonad (X)

launchApp ::  AL.Application -> X ()
launchApp = AL.launchApp def

xdgOpen :: X ()
xdgOpen = launchApp "xdg-open"

browser :: X ()
browser = launchApp "qutebrowser"
