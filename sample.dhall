let Config = ./dhall/Config.dhall

in
{ applications = [{ name = "foo", action = "asd"}]
, topics = [] : List ./dhall/TopicRule.dhall
, searchEndpoints = [] : List ./dhall/Search.dhall
} : Config
