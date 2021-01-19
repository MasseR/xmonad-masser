{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Control.Lens

import qualified Data.Map as M
import XMonad
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.TopicSpace
import XMonad.TopicUtils

import XMonad.Config.MasseR.ExtraConfig

import qualified Data.Text as T

import XMonad.Configurable

data TopicAction = TopicAction { _topicActionName :: String
                               , _topicActionAction :: X ()
                               , _topicActionHome :: Maybe FilePath }

makeFields ''TopicAction

addTopic :: TopicAction -> Configurable TopicConfig
addTopic t = EndoM $ \super -> let
  newDirs = topicDirs super <> foldOf (home . _Just . to (M.singleton (t ^. name))) t
  newActions = topicActions super <> M.singleton (t ^. name) (t ^. action)
  in pure super { topicDirs = newDirs
                , topicActions = newActions }

topicConfig :: ExtraConfig -> TopicConfig
topicConfig extraConfig =
  let dirs = M.fromList [ (T.unpack n, T.unpack d) | TopicRule n (Just d) _ <- extraConfig ^. topics ]
      actions = M.fromList [ (T.unpack n, spawn (T.unpack a)) | TopicRule n _ (Just a) <- extraConfig ^. topics ]
  in def {
    topicDirs = dirs
  , defaultTopicAction = const (realTopicDir dirs >>= spawnShellIn)
  , defaultTopic = "irc"
  , topicActions = actions
  }


spawnShellIn :: Dir -> X ()
spawnShellIn dir = safeRunInTerm dir Nothing



