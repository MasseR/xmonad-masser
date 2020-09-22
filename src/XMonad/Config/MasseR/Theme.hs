module XMonad.Config.MasseR.Theme where

import XMonad.Layout.Decoration
       (Theme(..), def)

-- Define the colors
zenburnFg1 :: String
zenburnFg1     = "#656555"
zenburnFg05 :: String
zenburnFg05    = "#989890"
zenburnFg :: String
zenburnFg       = "#DCDCCC"
zenburnFgU1 :: String
zenburnFgU1     = "#FFFFEF"
zenburnFgU2 :: String
zenburnFgU2     = "#FFFFFD"
zenburnBg2 :: String
zenburnBg2     = "#000000"
zenburnBg1 :: String
zenburnBg1     = "#2B2B2B"
zenburnBg08 :: String
zenburnBg08    = "#303030"
zenburnBg05 :: String
zenburnBg05    = "#383838"
zenburnBg :: String
zenburnBg       = "#3F3F3F"
zenburnBgU05 :: String
zenburnBgU05    = "#494949"
zenburnBgU1 :: String
zenburnBgU1     = "#4F4F4F"
zenburnBgU2 :: String
zenburnBgU2     = "#5F5F5F"
zenburnBgU3 :: String
zenburnBgU3     = "#6F6F6F"
zenburnRed6 :: String
zenburnRed6    = "#6C3333"
zenburnRed5 :: String
zenburnRed5    = "#7C4343"
zenburnRed4 :: String
zenburnRed4    = "#8C5353"
zenburnRed3 :: String
zenburnRed3    = "#9C6363"
zenburnRed2 :: String
zenburnRed2    = "#AC7373"
zenburnRed1 :: String
zenburnRed1    = "#BC8383"
zenburnRed :: String
zenburnRed      = "#CC9393"
zenburnRedU1 :: String
zenburnRedU1    = "#DCA3A3"
zenburnRedU2 :: String
zenburnRedU2    = "#ECB3B3"
zenburnOrange :: String
zenburnOrange   = "#DFAF8F"
zenburnYellow2 :: String
zenburnYellow2 = "#D0BF8F"
zenburnYellow1 :: String
zenburnYellow1 = "#E0CF9F"
zenburnYellow :: String
zenburnYellow   = "#F0DFAF"
zenburnGreen5 :: String
zenburnGreen5  = "#2F4F2F"
zenburnGreen4 :: String
zenburnGreen4  = "#3F5F3F"
zenburnGreen3 :: String
zenburnGreen3  = "#4F6F4F"
zenburnGreen2 :: String
zenburnGreen2  = "#5F7F5F"
zenburnGreen1 :: String
zenburnGreen1  = "#6F8F6F"
zenburnGreen :: String
zenburnGreen    = "#7F9F7F"
zenburnGreenU1 :: String
zenburnGreenU1  = "#8FB28F"
zenburnGreenU2 :: String
zenburnGreenU2  = "#9FC59F"
zenburnGreenU3 :: String
zenburnGreenU3  = "#AFD8AF"
zenburnGreenU4 :: String
zenburnGreenU4  = "#BFEBBF"
zenburnCyan :: String
zenburnCyan     = "#93E0E3"
zenburnBlueU3 :: String
zenburnBlueU3   = "#BDE0F3"
zenburnBlueU2 :: String
zenburnBlueU2   = "#ACE0E3"
zenburnBlueU1 :: String
zenburnBlueU1   = "#94BFF3"
zenburnBlue :: String
zenburnBlue     = "#8CD0D3"
zenburnBlue1 :: String
zenburnBlue1   = "#7CB8BB"
zenburnBlue2 :: String
zenburnBlue2   = "#6CA0A3"
zenburnBlue3 :: String
zenburnBlue3   = "#5C888B"
zenburnBlue4 :: String
zenburnBlue4   = "#4C7073"
zenburnBlue5 :: String
zenburnBlue5   = "#366060"
zenburnMagenta :: String
zenburnMagenta  = "#DC8CC3"

defaultTheme :: Theme
defaultTheme =
  def {
      activeBorderColor = zenburnFg
    , activeTextColor = zenburnFg
    , activeColor = zenburnBg
    , fontName = "xft:Inconsolata-9"
    , inactiveBorderColor = zenburnBgU3
    , inactiveTextColor = zenburnBgU1
    , inactiveColor = zenburnBgU3
  }

topBarTheme :: Theme
topBarTheme = def { activeColor = zenburnGreen }
