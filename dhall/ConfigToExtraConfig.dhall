let Config = ./package.dhall
let ExtraConfig = ./ExtraConfig.dhall
let convert : Config.Type -> ExtraConfig.Type =
  \(config : Config.Type) -> config with applications = ./ApplicationToMap.dhall config.applications
in convert
