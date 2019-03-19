{-# Language RecordWildCards #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
module Config where

import Dhall

data Applications = Applications { urxvt :: Text
                                 , prompt :: Text
                                 , browser :: Text
                                 , launcher :: Text -- xdg-open
                                 , screenshot :: Text
                                 }
                  deriving (Show, Generic)
data TopicRule = TopicRule { topicName :: Text
                           , topicHome :: Maybe Text
                           , topicAction :: Maybe Text }
               deriving (Show, Generic)
data ExtraConfig = ExtraConfig { applications :: Applications
                               , topics :: [TopicRule]}
                 deriving (Show, Generic)

defaultExtraConfig :: ExtraConfig
defaultExtraConfig =
    let applications = Applications{..}
        urxvt = "urxvt"
        prompt = "xmobar"
        browser = "qutebrowser"
        launcher = "xdg-open"
        screenshot = "scrot ~/screenSel.png"
        topics = []
    in ExtraConfig{..}

instance Interpret ExtraConfig
instance Interpret TopicRule
instance Interpret Applications
