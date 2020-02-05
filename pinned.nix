let
  rev    = "e9a21da6a1c1f4ecd3f40478dedd6d57ef903054";
  sha256 = "1ywmkacbva7qpwqprwgvzyl4a7fcygys12qj9cfsjwyfsbl39zf6";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import nixpkgs { config.allowUnfree = true; };
in pkgs

