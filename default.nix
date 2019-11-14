{ mkDerivation, aeson, base, brick, bytestring, Chart
, Chart-diagrams, colour, containers, data-default-class, directory
, file-embed, lens, microlens, microlens-th, optparse-applicative
, proteaaudio, random, random-shuffle, stdenv, strict, tasty
, tasty-hunit, text, time, vty
}:
mkDerivation {
  pname = "nback";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base brick bytestring Chart Chart-diagrams colour containers
    data-default-class directory file-embed lens microlens microlens-th
    proteaaudio random random-shuffle strict text time vty
  ];
  executableHaskellDepends = [
    aeson base brick bytestring containers directory microlens
    microlens-th optparse-applicative time vty
  ];
  testHaskellDepends = [ base containers tasty tasty-hunit ];
  doHaddock = false;
  doCheck = false;
  description = "See README for more info";
  license = stdenv.lib.licenses.mit;
}
