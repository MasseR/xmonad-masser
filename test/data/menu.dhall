let Config = ../../dhall/package.dhall

in  Config::{
    , applications = { terminal = "terminal", prompt = "prompt" }
    , menu =
          [ Config.Tree.node
              { name = "terminal"
              , extra = ""
              , value = Some (Config.Command.Spawn "asd")
              , children = [] : List Config.Tree.Type
              }
          ]
        : List Config.Tree.Type
    }
