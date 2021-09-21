let Make = ./Make.dhall

let SubCommand =
      forall (SubCommand : Type) ->
      forall (m : Make SubCommand -> SubCommand) ->
        SubCommand

in  SubCommand
