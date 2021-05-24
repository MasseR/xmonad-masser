{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module XMonad.Config.MasseR.ExtraConfig where

import Data.Maybe
       (fromMaybe)

import Control.Lens ( (^?), (^.), makeFields, Ixed(ix) )
import Data.Text.Strict.Lens
       (unpacked)


import Data.Map
       (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.Generics ( Generic )

data Application
  = Application { _applicationName :: Text
                , _applicationAction :: FilePath
                }
  deriving stock (Show, Generic)

makeFields ''Application

data TopicRule =
  TopicRule { _topicRuleName :: Text
            , _topicRuleHome :: Maybe Text
            , _topicRuleAction :: Maybe Text
            , _topicRuleClasses :: [Text]
            }
  deriving stock (Show, Generic)

makeFields ''TopicRule

data Search = Search { _searchName :: String
                     , _searchKey :: String
                     , _searchUrl :: String
                     }
  deriving stock (Show, Generic)

makeFields ''Search

data ExtraConfig =
  ExtraConfig { _extraConfigApplications :: Map Text Application
              , _extraConfigTopics :: [ TopicRule ]
              , _extraConfigSearchEndpoints :: [Search]
              }
  deriving (Show, Generic)

makeFields ''ExtraConfig

instance Semigroup ExtraConfig where
  a <> b =
    ExtraConfig { _extraConfigApplications = M.union (a ^. applications) (b ^. applications)
                , _extraConfigTopics = a ^. topics <> b ^. topics
                , _extraConfigSearchEndpoints = a ^. searchEndpoints <> b ^. searchEndpoints
                }

instance Monoid ExtraConfig where
  mempty = ExtraConfig M.empty mempty mempty

type Action = String

application :: ExtraConfig -> Text -> FilePath
application c n = fromMaybe ("xmessage '" <> n ^. unpacked <> "' not found") $ c ^? applications . ix n . action
