let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          proteaaudio =
            haskellPackagesNew.callPackage ./nix/proteaaudio.nix {};
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  pkgs.haskellPackages.callPackage ./default.nix {}
