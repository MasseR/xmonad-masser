{-# LANGUAGE LambdaCase #-}
module XMonad.XMobar (zenburnPP) where

-- import XMonad.Util.Loggers
import XMonad.Hooks.DynamicLog (
    PP(..)
  , xmobarColor
  , shorten
  , xmobarPP
  , dzenStrip)


myUrgencyHintFgColor :: String
myUrgencyHintFgColor = "#333333"

myUrgencyHintBgColor :: String
myUrgencyHintBgColor = "#F18C96"


-- Xmobar pretty printer. Color scheme zenburn
zenburnPP :: PP
zenburnPP = xmobarPP {
      ppTitle = xmobarColor "#DCA3A3" "" . shorten 70
    , ppCurrent = xmobarColor "#CEFFAC" ""
    , ppHidden = const ""
    , ppSep = " | "
    , ppLayout = id
    , ppUrgent = xmobarColor myUrgencyHintFgColor myUrgencyHintBgColor . dzenStrip
    , ppOrder = \case
      (ws:_layout:_title:_) -> [ws]
      _ -> []
    , ppExtras = []
  }

-- xmobar :: IO (X ())
-- xmobar = do
--   xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar"
--   return $ dynamicLogWithPP $ zenburnPP xmproc
