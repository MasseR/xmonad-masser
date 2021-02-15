let Config = ./package.dhall
in
{ Type =
  { searchEndpoints : List Config.Search.Type
  , topics : List Config.TopicRule.Type
  , applications : List { mapKey : Text, mapValue : Config.Application.Type }
  }
, default =
  { searchEndpoints = [] : List Config.Search.Type
  , topics = [] : List Config.TopicRule.Type
  }
}
