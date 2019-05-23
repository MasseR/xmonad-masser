{-# Language RecordWildCards #-}
module XMonad.TopicSpace (topicKeys', addTopic, TopicAction(..)) where

import XMonad.Actions.TopicSpace
import XMonad
import qualified Data.Map as M
import XMonad.Actions.DynamicWorkspaces
import XMonad.TopicUtils
import XMonad.Util.EZConfig (mkNamedKeymap)
import XMonad.Util.NamedActions

import XMonad.Config.MasseR.ExtraConfig

import qualified Data.Text as T

import XMonad.Configurable

data TopicAction = TopicAction { name :: String
                               , action :: X ()
                               , home :: Maybe FilePath }

addTopic :: TopicAction -> Configurable TopicConfig
addTopic TopicAction{..} = EndoM $ \super -> let
  newDirs = topicDirs super <> maybe mempty (M.singleton name) home
  newActions = topicActions super <> M.singleton name action
  in pure super { topicDirs = newDirs
                , topicActions = newActions }

myTopicConfig :: ExtraConfig -> TopicConfig
myTopicConfig extraConfig =
  let dirs = M.fromList [ (T.unpack n, T.unpack d) | TopicRule n (Just d) _ <- topics extraConfig ]
      actions = M.fromList [ (T.unpack n, spawn (T.unpack a)) | TopicRule n _ (Just a) <- topics extraConfig ]
  in def {
    topicDirs = dirs
  , defaultTopicAction = const (realTopicDir dirs >>= spawnShellIn)
  , defaultTopic = "irc"
  , topicActions = actions
  }


topicKeys' :: ExtraConfig -> XConfig l -> [(String, NamedAction)]
topicKeys' extraConfig conf = [ ("M-y", addName "Change topic" $ visualSelect (myTopicConfig extraConfig))
                              , ("M-S-g", addName "Move window to topic" $ gridselectMove def)
                              , ("M-<Return>", addName "Open project action" $ currentTopicAction' (myTopicConfig extraConfig))
                              , ("M-w", modificationSubmaps' conf)]


spawnShellIn :: Dir -> X ()
spawnShellIn dir = safeRunInTerm dir Nothing


modificationSubmaps' :: XConfig l -> NamedAction
modificationSubmaps' conf =
    submapName $ mkNamedKeymap conf [ ("a", addName "Add a new workspace" $ addWorkspacePrompt def)
                                    , ("w", addName "Copy project" copyTopic)
                                    , ("d", addName "Remove empty workspace" removeEmptyWorkspace)]

