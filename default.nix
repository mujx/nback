{ mkDerivation, aeson, base, brick, bytestring, Chart
, Chart-diagrams, colour, containers, data-default-class, directory
, file-embed, microlens, microlens-th, optparse-applicative
, proteaaudio, random, random-shuffle, stdenv, strict, tasty
, tasty-hunit, text, time, vty
}:
mkDerivation {
  pname = "nback";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base brick bytestring Chart Chart-diagrams colour containers
    data-default-class directory file-embed microlens microlens-th
    optparse-applicative proteaaudio random random-shuffle strict text
    time vty
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory file-embed proteaaudio
    random random-shuffle strict tasty tasty-hunit time
  ];
  doHaddock = false;
  doCheck = false;
  description = "See README for more info";
  license = stdenv.lib.licenses.unlicense;
}
