{-# LANGUAGE RecordWildCards #-}
module XMonad.TopicSpace
  ( visualSelect
  , gridselectMove
  , currentTopicAction'
  , addWorkspacePrompt
  , copyTopic
  , removeEmptyWorkspace
  , topicConfig
  , addTopic
  , TopicAction(..)
  )
  where

import qualified Data.Map as M
import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.TopicSpace
import XMonad.TopicUtils

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

topicConfig :: ExtraConfig -> TopicConfig
topicConfig extraConfig =
  let dirs = M.fromList [ (T.unpack n, T.unpack d) | TopicRule n (Just d) _ <- topics extraConfig ]
      actions = M.fromList [ (T.unpack n, spawn (T.unpack a)) | TopicRule n _ (Just a) <- topics extraConfig ]
  in def {
    topicDirs = dirs
  , defaultTopicAction = const (realTopicDir dirs >>= spawnShellIn)
  , defaultTopic = "irc"
  , topicActions = actions
  }


spawnShellIn :: Dir -> X ()
spawnShellIn dir = safeRunInTerm dir Nothing



