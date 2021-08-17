{ pkgs ? import ./nix/nixpkgs.pinned.nix 
}: import ./nix/build.nix { inherit pkgs; }

