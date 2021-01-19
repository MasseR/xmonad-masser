let Application = ./Application.dhall
let List/map = https://prelude.dhall-lang.org/v20.0.0/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let appToMap : List Application -> List { mapKey : Text, mapValue : Application } =
  List/map Application { mapKey : Text, mapValue : Application } (\(app : Application) -> { mapKey = app.name, mapValue = app })

in appToMap
