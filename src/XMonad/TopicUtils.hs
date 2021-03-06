{-# LANGUAGE TupleSections #-}
module XMonad.TopicUtils where

import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.List (isPrefixOf, nub, sortOn)
import XMonad.Actions.TopicSpace
import XMonad
import qualified Data.Map as M
import XMonad.Actions.GridSelect hiding (gridselectWorkspace)
import XMonad.Util.Run (safeSpawn)
import qualified XMonad.StackSet as W
import XMonad.Util.Dmenu (dmenu)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Util.NamedWindows (getName)
import Data.Ord


realTopicDir :: M.Map String FilePath -> X String
realTopicDir tg = do
  topic <- realTopic
  return . fromMaybe "" . M.lookup topic $ tg


safeRunInTerm :: Dir -> Maybe String -> X ()
safeRunInTerm dir Nothing = safeSpawn "urxvt" ["-cd", dir]
safeRunInTerm dir (Just command) = safeSpawn "urxvt" ["-cd", dir, "-e", command]

inactiveTags :: X [WorkspaceId]
inactiveTags = inactive' <$> gets windowset
    where
        inactive' s =
          let current = W.currentTag s
          in filter (current /=) . map W.tag $ W.hidden s <> map W.workspace (W.visible s)

gridselectMove :: GSConfig WorkspaceId -> X ()
gridselectMove conf = do
    topics <- inactiveTags
    gridselect conf [(x,x) | x <- topics] >>= maybe (return ()) (windows . W.shift)

dmenuMove :: X ()
dmenuMove = do
  topics <- inactiveTags
  dmenu topics >>= \t -> windows (W.shift t)

gsConfig :: GSConfig Window
gsConfig = def{gs_navigate = navNSearch, gs_colorizer = fromClassName}

-- Copied from gridselect and modified so that it doesn't contain current and visible
-- - Doesn't contain current and visible
-- - Takes a topicspace viewfunc
gridselectWorkspace :: GSConfig WorkspaceId -> (WorkspaceId -> X ()) -> X ()
gridselectWorkspace conf viewFunc = withWindowSet $ \ws -> do
  let wss = filter (/= current) . map W.tag $ invisible <> visible
      visible = fmap W.workspace (W.visible ws)
      invisible = W.hidden ws
      current = W.currentTag ws
  gridselect conf (zip wss wss) >>= flip whenJust viewFunc

gotoSelected' :: GSConfig Window -> X ()
gotoSelected' gsconf = do
    w <- gridselectCurrentWindows gsconf
    maybe (return ()) (windows . W.focusWindow) w

gridselectCurrentWindows :: GSConfig Window -> X (Maybe Window)
gridselectCurrentWindows gsconf = windowMap >>= gridselect gsconf
    where
        getName' = fmap show . getName
        kvPair w = (, w) <$> getName' w
        windowMap = do
            ws <- gets (nub . W.integrate' . W.stack . W.workspace . W.current . windowset)
            mapM kvPair ws

visualSelect :: TopicConfig -> X ()
visualSelect cfg = gridselectWorkspace def{gs_navigate = navNSearch, gs_colorizer = stringColorizer} (switchTopic cfg)

realTopic :: X String
realTopic = gets (real . W.tag . W.workspace . W.current . windowset)
  where real = takeWhile (/= ':')

currentTopicAction' :: TopicConfig -> X ()
currentTopicAction' tg = do
  topic <- realTopic
  topicAction tg topic

copyTopic :: X ()
copyTopic = do
  currentTopic <- realTopic
  lastN <- gets (listToMaybe . sortOn Down . mapMaybe (subset currentTopic . W.tag) . W.workspaces . windowset)
  addWorkspace (currentTopic ++ ":" ++ show (maybe 2 (+1) lastN))
  where
    subset :: String -> String -> Maybe Int
    subset topic other = if topic `isPrefixOf` other then readM $ tail' $ dropWhile (/= ':') other else Nothing
    readM a = case reads a of
               [(x,_)] -> Just x
               _ -> Nothing
    tail' [] = []
    tail' xs = tail xs
