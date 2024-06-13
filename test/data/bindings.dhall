let Config = ../../dhall/package.dhall

in  Config::{
    , applications = { terminal = "terminal", prompt = "prompt" }
    , bindings =
          [ Config.SubCommand.subMap
              { name = "music"
              , prefix = "M-m"
              , sub =
                    [ Config.SubCommand.act
                        { prefix = "M-x"
                        , name = "asd"
                        , command = Config.Command.Spawn "foo"
                        }
                    ]
                  : List Config.SubCommand.Type
              }
          ]
        : List Config.SubCommand.Type
    }
