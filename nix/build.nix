{ pkgs ? import ./nixpkgs.pinned.nix 
}: let
  hPkgs = pkgs.haskell.packages.ghc8104;
in { inherit hPkgs; } // hPkgs.callCabal2nix "fsynth" ../. {}

