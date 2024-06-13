{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
module XMonad.Config.MasseR.ExtraConfig where

import qualified Data.Fix as Fix
import Data.Fix (Fix)


import Control.Lens (makeFields, toListOf, folded, (^.))


import Data.Text (Text)
import GHC.Generics ( Generic (Rep), from, to )
import Data.Coerce (Coercible, coerce)
import XMonad.Prompt (XPConfig)
import XMonad (XConfig, spawn, KeyMask, KeySym, def, X)
import XMonad.Util.NamedActions (NamedAction, addName, subtitle, submapName)
import XMonad.Actions.Search (promptSearchBrowser, searchEngine)

import Dhall.Deriving
import Dhall (ToDhall, FromDhall)
import qualified Dhall.Core
import qualified Dhall
import XMonad.Util.EZConfig (mkNamedKeymap)
import System.Environment (getEnv)
import Data.Semigroup (Last(Last))
import XMonad.Actions.TopicSpace (TopicConfig(..))
import qualified Data.Map.Strict as M
import GHC.TypeLits (Symbol)
import Data.Foldable (traverse_)

newtype Isomorphic a b = Isomorphic a

type GenericCoercible a b =
  ( Generic a
  , Generic b
  , Coercible (Rep a ()) (Rep b ())
  )

genericCoerce :: forall a b. GenericCoercible a b => a -> b
genericCoerce = to . (coerce @(Rep a ()) @(Rep b ())) . from

instance (Semigroup b, GenericCoercible a b) => Semigroup (Isomorphic a b) where
  Isomorphic a <> Isomorphic b = Isomorphic (genericCoerce @b @a (genericCoerce a <> genericCoerce b))

newtype Path = Path { getPath :: String }
  deriving Show

instance Semigroup Path where
  Path a <> Path b = Path (a <> ":" <> b)

type C (prefix :: Symbol) = Codec (Field (CamelCase <<< DropPrefix prefix))

data Applications = Applications
  { applicationsTerminal :: FilePath
  , applicationsPrompt :: FilePath
  }
  deriving (Generic, Show)
  deriving (ToDhall, FromDhall) via (C "applications" Applications)

makeFields ''Applications

data Engine = Engine
  { engineName :: String
  , engineBrowser :: FilePath
  , engineSite :: String
  }
  deriving (Generic, Show)
  deriving (ToDhall, FromDhall) via (C "engine" Engine)

makeFields ''Engine

data Command = Spawn String | Search Engine
  deriving (Generic, Show)
  deriving (ToDhall, FromDhall) via (Codec AsIs Command)

data Topic = Topic
  { topicName :: String
  , topicAction :: [Command]
  , topicHome :: Maybe FilePath
  }
  deriving (Generic, Show)
  deriving (ToDhall, FromDhall) via (C "topic" Topic)

makeFields ''Topic

data SubCommandF a k
  = ActF { subCommandPrefixF :: String, subCommandNameF :: String, subCommandCommandF :: Command }
  | SubMapF { subCommandPrefixF :: String, subCommandNameF :: String, subCommandSubF :: [k] }
  deriving Functor

deriving instance Generic (SubCommandF a x)
deriving via (C "subCommand" (SubCommandF a x)) instance (ToDhall a, ToDhall x) => ToDhall (SubCommandF a x)
deriving via (C "subCommand" (SubCommandF a x)) instance (FromDhall a, FromDhall x) => FromDhall (SubCommandF a x)

evalCommand :: XPConfig -> Command -> X ()
evalCommand xp = \case
  Spawn cmd -> spawn cmd
  Search Engine{..} -> promptSearchBrowser xp engineBrowser (searchEngine engineName engineSite)

mkCommandF
  :: XPConfig
  -> XConfig l
  -> SubCommandF String [((KeyMask, KeySym), NamedAction)]
  -> [((KeyMask, KeySym), NamedAction)]
mkCommandF xp conf = \case
  ActF { subCommandPrefixF = prefix, subCommandNameF = cmdName, subCommandCommandF = cmd } ->
    mkNamedKeymap conf [(prefix, addName cmdName $ evalCommand xp cmd)]
  SubMapF { subCommandPrefixF = prefix, subCommandNameF = cmdName, subCommandSubF = sub } ->
    subtitle cmdName : mkNamedKeymap conf [(prefix, submapName (concat sub))]

fold :: Functor f => (f a -> a) -> Fix.Fix f -> a
fold f = c where c = f . fmap c . Fix.unFix
{-# INLINE fold #-}

mkCommand :: XPConfig -> XConfig l -> Fix.Fix (SubCommandF String) -> [((KeyMask, KeySym), NamedAction)]
mkCommand xp conf = fold (mkCommandF xp conf)

mkBindings :: Config -> XPConfig -> XConfig l -> [((KeyMask, KeySym), NamedAction)]
mkBindings c xp xc = concatMap (mkCommand xp xc) (configBindings c)


data Config = Config
  { configPath :: String -- ^ the path containing the binaries
  , configTopics :: [Topic]
  , configBindings :: [Fix (SubCommandF String)]
  , configApplications :: Applications
  }
  deriving stock (Generic)
  deriving (Semigroup) via (Config `Isomorphic` (Path, Last [Topic], [ Fix (SubCommandF String) ], Last Applications))
  deriving (ToDhall, FromDhall) via (C "config" Config)

makeFields ''Config

mkWorkspaces :: Config -> [String]
mkWorkspaces = toListOf (topics . folded . name)

mkTopicConfig :: XPConfig -> Config -> TopicConfig
mkTopicConfig xp c = def
  { topicDirs = M.fromList [(k,v) | Topic{topicName=k,topicHome=Just v} <- c ^. topics]
  , topicActions = M.fromList [(k,traverse_ (evalCommand xp) v) | Topic{topicName=k,topicAction=v} <- c ^. topics]
  }

defaultConfig :: IO Config
defaultConfig = x <$> getEnv "PATH"
  where
  x p = Config
    { configPath = p
    , configTopics = [ Topic "web" [] Nothing ]
    , configBindings = []
    , configApplications = Applications { applicationsTerminal = "urxvt", applicationsPrompt = "xmobar" }
    }


prettyPrintDhall :: forall a. Dhall.FromDhall a => Dhall.Expector Text
prettyPrintDhall = fmap Dhall.Core.pretty (Dhall.expected (Dhall.auto @a))
