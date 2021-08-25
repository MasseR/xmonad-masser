let Command = ../Command/Type.dhall

let Make = ./Make.dhall

let SubCommand = ./Type.dhall

let Act
    : { prefix : Text, name : Text, command : Command } ->
        SubCommand
    = \(cmd : { prefix : Text, name : Text, command : Command }) ->
      \(result : Type) ->
      \(make : Make result -> result) ->
        make
          ( (Make result).ActF
              { prefixF = cmd.prefix, nameF = cmd.name, commandF = cmd.command }
          )

in  Act
