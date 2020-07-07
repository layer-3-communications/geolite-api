{ mkDerivation, aeson, attoparsec, base, bytestring, criterion
, deepseq, doctest, hashable, hspec, hspec-discover, HUnit
, primitive, QuickCheck, quickcheck-classes, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, vector
, wide-word
}:
mkDerivation {
  pname = "ip";
  version = "1.5.1";
  sha256 = "94fec997abbd2a877123e27a3abf2cb123b8ce28805953ca0818c3becdb136b3";
  libraryHaskellDepends = [
    aeson attoparsec base bytestring deepseq hashable primitive text
    vector wide-word
  ];
  testHaskellDepends = [
    attoparsec base bytestring doctest hspec HUnit QuickCheck
    quickcheck-classes test-framework test-framework-hunit
    test-framework-quickcheck2 text wide-word
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring criterion text
  ];
  homepage = "https://github.com/andrewthad/haskell-ip#readme";
  description = "Library for IP and MAC addresses";
  license = stdenv.lib.licenses.bsd3;
}
