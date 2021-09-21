\(sub : Type) ->
  < ActF : { prefixF : Text, nameF : Text, commandF : ../Command/Type.dhall }
  | SubMapF : { prefixF : Text, nameF : Text, subF : List sub }
  >
