{ searchEndpoints : List ./Search.dhall
, topics : List ./TopicRule.dhall
, applications : List { mapKey : Text, mapValue : ./Application.dhall }
}

