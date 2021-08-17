{ pkgs ? import ./nixpkgs.pinned.nix
}: drv: with drv.hPkgs; let
  name = drv.name;
  ghc = ghcWithPackages (pkgs: (drv.propagatedBuildInputs ++ drv.buildInputs));
  cabalShim = pkgs.writeScriptBin "cabal" ''
    cleanup() {
      rm -f ${name}.cabal
    }
    trap cleanup SIGINT
    ${hpack}/bin/hpack && PATH=${ghc}/bin:$PATH ${cabal-install}/bin/cabal $@
    cleanup
  '';
  repl = pkgs.writeScriptBin "repl" ''
    ${cabalShim}/bin/cabal repl $@
  '';
  watch = pkgs.writeScriptBin "watch" ''
    ${pkgs.ghcid}/bin/ghcid -c "${repl}/bin/repl $@"
  '';
  run = pkgs.writeScriptBin "run" ''
    ${cabalShim}/bin/cabal v2-run
  '';
in pkgs.mkShell rec {
  inherit name;
  buildInputs = [
    cabalShim
    repl
    watch
    run
    haskell-language-server
  ];
  HISTFILE = toString ../.history;
  LOCAL_HISTFILE = HISTFILE;
}

