{-# LANGUAGE DeriveGeneric #-}
module XMonad.Config.MasseR.ExtraConfig where

import GHC.Generics (Generic)
import Data.Text (Text)

data Applications =
  Applications { browser :: FilePath
               , launcher :: FilePath
               , prompt :: FilePath
               , screenshot :: FilePath
               , urxvt :: FilePath
               , musicToggle :: FilePath
               } deriving (Show, Generic)

data TopicRule =
  TopicRule { topicName :: Text
            , topicHome :: Maybe Text
            , topicAction :: Maybe Text
            }
               deriving (Show, Generic)

data Search = Search { name :: String
                     , key :: String
                     , url :: String
                     }
            deriving (Show, Generic)

data ExtraConfig =
  ExtraConfig { applications :: Applications
              , topics :: [ TopicRule ]
              , searchEndpoints :: [Search] }
                 deriving (Show, Generic)
