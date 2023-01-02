{ buildDhallDirectoryPackage, Prelude }:
  buildDhallDirectoryPackage {
    name = "";
    src = ./dhall;
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [ (Prelude.overridePackage { file = "package.dhall"; }) ];
    }
