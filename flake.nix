# https://docs.haskellstack.org/en/stable/topics/nix_integration/
{
  description = "Fachprüfungsordnungseditor";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      easy-purescript-nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        welcomeText = mode: ''
            __
           / _|
          | |_ _ __   ___
          |  _| '_ \ / _ \\
          | | | |_) | (_) |
          |_| | .__/ \___(_)
              | | Dev Shell
              |_| ${mode}

        '';

        pkgs = nixpkgs.legacyPackages.${system};
        easyPs = easy-purescript-nix.packages.${system};

        hPkgs = pkgs.haskell.packages."ghc984";
        # need to match Stackage LTS version
        # from stack.yaml snapshot

        backendDevTools = [
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          hPkgs.ghcid # Continuous terminal Haskell compile checker
          hPkgs.ormolu # Haskell formatter
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          hPkgs.retrie # Haskell refactoring tool
          # hPkgs.cabal-install
          stack-wrapped
          pkgs.zlib # External C library needed by some Haskell packages
          pkgs.postgresql
        ];

        frontendDevTools = [
          easyPs.purs
          easyPs.spago
          easyPs.purescript-language-server
          easyPs.purs-tidy
          pkgs.purescript
          pkgs.nodePackages.purescript-language-server
          pkgs.nodePackages.purs-tidy
          pkgs.nodejs_22
          pkgs.esbuild
        ];

        frontendShellHook = ''
          source <(spago --bash-completion-script `which spago`)
          source <(node --completion-bash)
        '';

        # Wrap Stack to work with our Nix integration. We do not want to modify
        # stack.yaml so non-Nix users do not notice anything.
        # - no-nix: We do not want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Do not try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in
      {
        devShells.frontend = pkgs.mkShell {
          buildInputs = frontendDevTools;
          shellHook = ''
            echo "${welcomeText "Frontend"}"
            ${frontendShellHook}
          '';
        };

        devShells.backend = pkgs.mkShell {
          buildInputs = backendDevTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath backendDevTools;
          shellHook = ''
            echo "${welcomeText "Backend"}"
          '';
        };

        devShells.default = pkgs.mkShell {
          buildInputs = backendDevTools ++ frontendDevTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath backendDevTools;
          shellHook = ''
            echo "${welcomeText "Fullstack"}"
            ${frontendShellHook}
          '';
        };
      }
    );
}
