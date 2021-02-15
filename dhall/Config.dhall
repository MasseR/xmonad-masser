let Search = ./Search.dhall
let TopicRule = ./TopicRule.dhall
let Application = ./Application.dhall
in
{ Type =
  { searchEndpoints : List Search.Type
  , topics : List TopicRule.Type
  , applications : List Application.Type
  }
, default =
  { searchEndpoints = [] : List Search.Type
  , topics = [] : List TopicRule.Type
  , applications = [] : List Application.Type
  }
}
