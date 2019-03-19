module Screen (screenWidth, nextScreen, prevScreen) where

import Graphics.X11.Xinerama (xineramaQueryScreens, XineramaScreenInfo(..))
import Graphics.X11.Xlib (openDisplay, closeDisplay)
import qualified XMonad.Actions.CycleWS as CycleWS
import XMonad (X())
import Control.Monad.Trans (liftIO)

screenWidth :: Int -> IO Int
screenWidth idx = do
  dsp <- openDisplay "" -- I don't know what the string does :/
  info <- xineramaQueryScreens dsp
  case info of
       Nothing -> return 0
       Just [] -> return 0
       Just screens -> if idx >= 0 && idx < length screens
                          then (return . fromIntegral . xsi_width) (screens !! idx)
                          else return 0

swapScreenDir :: IO Bool
swapScreenDir = do
  dsp <- openDisplay ""
  info <- xineramaQueryScreens dsp
  closeDisplay dsp
  case info of
       Nothing -> return False
       Just [] -> return False
       Just (s:_) -> return (xsi_width s > 0)

nextScreen :: X ()
nextScreen = do
  swap <- liftIO $ swapScreenDir -- Not too fond of querying the x each time we swap screens, but oh well
  if swap then CycleWS.prevScreen else CycleWS.nextScreen

prevScreen :: X ()
prevScreen = do
  swap <- liftIO $ swapScreenDir -- Not too fond of querying the x each time we swap screens, but oh well
  if swap then CycleWS.nextScreen else CycleWS.prevScreen
