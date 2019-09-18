{ mkDerivation, base, bytestring, c2hs, stdenv, lib, darwin }:
mkDerivation {
  pname = "proteaaudio";
  version = "0.7.1.0";
  sha256 = "37e24a9914ba9530adfec67c1f6cfac6ffabdc388d4d7559d9d17ace8f67aed6";
  isLibrary = true;
  isExecutable = true;
  librarySystemDepends = []
    ++ lib.optional stdenv.isDarwin darwin.apple_sdk.frameworks.AudioToolbox;
  libraryHaskellDepends = [ base bytestring ];
  libraryToolDepends = [ c2hs ];
  description = "Simple audio library for Windows, Linux, OSX";
  license = stdenv.lib.licenses.bsd3;
}
