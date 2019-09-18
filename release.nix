let
  pkgs = import <nixpkgs> {};
  proteaaudio = pkgs.haskellPackages.callPackage ./nix/proteaaudio.nix {};
in
  pkgs.haskellPackages.callPackage ./default.nix { inherit proteaaudio; }
