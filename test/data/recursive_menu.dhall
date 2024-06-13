let Config = ../../dhall/package.dhall

in  Config::{
    , applications = { terminal = "terminal", prompt = "prompt" }
    , menu =
          [ Config.Tree.node
              { name = "music"
              , extra = ""
              , value = None Config.Command
              , children =
                [ Config.Tree.node
                    { name = "Play previous"
                    , extra = ""
                    , value = Some (Config.Command.Spawn "sp prev")
                    , children = [] : List Config.Tree.Type
                    }
                , Config.Tree.node
                    { name = "Play next"
                    , extra = ""
                    , value = Some (Config.Command.Spawn "sp next")
                    , children = [] : List Config.Tree.Type
                    }
                ]
              }
          ]
        : List Config.Tree.Type
    }
