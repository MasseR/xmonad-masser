let Prelude =
      https://prelude.dhall-lang.org/v20.1.0/package.dhall
        sha256:26b0ef498663d269e4dc6a82b0ee289ec565d683ef4c00d0ebdd25333a5a3c98

let Command = ../Command/Type.dhall

let Make = ./Make.dhall

let Tree = ./Type.dhall

let Node
    : { name : Text, extra : Text, value : Optional Command, children : List Tree } →
        Tree
    = λ ( node
        : { name : Text, extra : Text, value : Optional Command, children : List Tree }
        ) →
      λ(result : Type) →
      λ(make : Make result → result) →
        make
          { nameF = node.name
          , extraF = node.extra
          , valueF = node.value
          , childrenF =
                Prelude.List.map
                  Tree
                  result
                  (λ(s : Tree) → s result make)
                  node.children
              : List result
          }

in  Node
