{
  description = "Logging frontend for Polysemy";

  inputs.hix.url = "github:tek/hix?ref=0.7.1";

  outputs = {hix, ...}: hix.lib.flake {
    overrides = { hackage, unbreak, notest, jailbreak, ... }: {
      polysemy-test = unbreak jailbreak;
      polysemy-conc = jailbreak;
      polysemy-log = jailbreak;
    };

    hackage.versionFile = "ops/version.nix";

    envs.dev = {
      setup = ''
        zsh
      '';

      haskellTools = ghc: with ghc; [
        fourmolu
      ];
    };

    cabal = {
      license = "MIT";
      license-file = "LICENSE";
      author = "James Burton";
      copyright = "2024 Functional Core";
      ghc-options = ["-Wall" "-O2"];
      language = "GHC2021";
    };

    packages.sem-log = {
      src = ./.;
      cabal.meta.synopsis = "Logging frontend for Polysemy";

      library = {
        enable = true;
        dependencies = [
          "aeson"
          "envy"
          "incipit"
          "polysemy"
          "polysemy-log"
          "polysemy-plugin"
          "text"
          "time"
        ];
        default-extensions = [
          "BlockArguments"
          "DataKinds"
          "DefaultSignatures"
          "DeriveAnyClass"
          "DerivingStrategies"
          "DuplicateRecordFields"
          "FunctionalDependencies"
          "GADTs"
          "ImportQualifiedPost"
          "LambdaCase"
          "MultiWayIf"
          "NoImplicitPrelude"
          "OverloadedRecordDot"
          "OverloadedStrings"
          "PartialTypeSignatures"
          "PatternSynonyms"
          "QuasiQuotes"
          "TemplateHaskell"
          "TypeFamilies"
          "TypeFamilyDependencies"
        ];
        ghc-options = [
            "-flate-specialise"
            "-fspecialise-aggressively"
            "-fplugin=Polysemy.Plugin"
        ];
        component = {
          other-modules = [
            "Core"
          ];
        };
      };

      executable.enable = false;

      test = {
        enable = true;
        dependencies = [
          "hedgehog >= 1.1 && < 1.5"
          "tasty ^>= 1.4"
          "tasty-hedgehog >= 1.3 && < 1.5"
        ];
      };

    };
  };
}
