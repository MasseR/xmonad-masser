let Prelude =
      https://prelude.dhall-lang.org/v20.1.0/package.dhall
        sha256:26b0ef498663d269e4dc6a82b0ee289ec565d683ef4c00d0ebdd25333a5a3c98

let Make = ./Make.dhall

let SubCommand = ./Type.dhall

let SubMap
    : { prefix : Text, name : Text, sub : List SubCommand } ->
        SubCommand
    = \(cmd : { prefix : Text, name : Text, sub : List SubCommand }) ->
      \(result : Type) ->
      \(make : Make result -> result) ->
        make
          ( (Make result).SubMapF
              { prefixF = cmd.prefix
              , nameF = cmd.name
              , subF =
                  Prelude.List.map
                    SubCommand
                    result
                    (\(s : SubCommand) -> s result make)
                    cmd.sub
              }
          )

in  SubMap
