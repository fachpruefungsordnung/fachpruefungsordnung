# https://docs.haskellstack.org/en/stable/topics/nix_integration/
{
  description = "Fachprüfungsordnungseditor";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
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
          pkgs.postgresql.dev
          pkgs.postgresql.pg_config
          pkgs.sql-formatter
          pkgs.pkg-config
          pkgs.gcc
          # python
          (pkgs.python312.withPackages (python-pkgs: [
            python-pkgs.requests
          ]))
          pkgs.basedpyright
        ];

        frontendDevTools = [
          pkgs.purescript
          pkgs.nodejs_22
          pkgs.esbuild

          (pkgs.writeShellScriptBin "spago" ''
            exec ${pkgs.nodejs_22}/bin/npx spago@next "$@"
          '')

          (pkgs.writeShellScriptBin "purescript-language-server" ''
            exec ${pkgs.nodejs_22}/bin/npx purescript-language-server "$@"
          '')

          (pkgs.writeShellScriptBin "purs-tidy" ''
            exec ${pkgs.nodejs_22}/bin/npx purs-tidy "$@"
          '')
        ];

        # ja ja ist vollkatastrophe, aber bleibt jetzt erst mal so.
        frontendShellHook = ''
          mkdir -p ./frontend/output
          ln -s ./frontend/spago.yaml spago.yaml > /dev/null 2>&1
          ln -s ./frontend/output output > /dev/null 2>&1
          cd ./frontend
          npm install && npm install spago@next && npm install purs-tidy \
            && npm install purescript-language-server && spago install && clear
          cd ..
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

        devShells.default = pkgs.mkShell {
          buildInputs = backendDevTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath backendDevTools;
          shellHook = ''
            echo "${welcomeText "Backend"}"
          '';
        };

        devShells.fullstack = pkgs.mkShell {
          buildInputs = backendDevTools ++ frontendDevTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath backendDevTools;
          shellHook = ''
            ${frontendShellHook}
            echo "${welcomeText "Fullstack"}"
          '';
        };
      }
    );
}
