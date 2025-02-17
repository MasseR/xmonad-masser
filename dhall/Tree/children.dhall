let Prelude =
      https://prelude.dhall-lang.org/v20.1.0/package.dhall
        sha256:26b0ef498663d269e4dc6a82b0ee289ec565d683ef4c00d0ebdd25333a5a3c98

let Make = ./Make.dhall

let Tree = ./Type.dhall

let SubForest
    : List Tree ->
        Tree
    = \(children : List Tree) ->
      \(result : Type) ->
      \(make : Make result -> result) ->
        make
          ( (Make result).SubForestF
              { childrenF =
                  Prelude.List.map
                    Tree
                    result
                    (\(s : Tree) -> s result make)
                    children
              }
          )

in  SubForest

