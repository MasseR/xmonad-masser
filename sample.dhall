let Config = ./dhall/package.dhall

in
Config::
  { applications = [ Config.Application::{ name = "foo", action = "asd" }]
  }
