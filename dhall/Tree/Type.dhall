let Make = ./Make.dhall

let Tree =
      forall (Tree : Type) ->
      forall (m : Make Tree -> Tree) ->
        Tree

in  Tree
