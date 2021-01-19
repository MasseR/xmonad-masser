let Config = ./Config.dhall
let convert : Config -> ./ExtraConfig.dhall =
  \(config : Config) -> config with applications = ./ApplicationToMap.dhall config.applications
in convert
