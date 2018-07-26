{ mkDerivation, aeson, attoparsec, base, bytestring, criterion
, doctest, fetchgit, ghc-prim, hashable, hspec, HUnit, primitive
, QuickCheck, quickcheck-classes, semigroups, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text, vector
}:
mkDerivation {
  pname = "ip";
  version = "1.2.1";
  src = fetchgit {
    url = "https://github.com/andrewthad/haskell-ip";
    sha256 = "1zj233yb3a0sj5a2x71jhic6gl08s0gxxg4bcvyf8a7g7fh94js7";
    rev = "9959a5137566c66194daca82848542f29738e3d2";
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring ghc-prim hashable primitive
    semigroups text vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring doctest hspec HUnit QuickCheck
    quickcheck-classes test-framework test-framework-hunit
    test-framework-quickcheck2 text
  ];
  benchmarkHaskellDepends = [
    attoparsec base bytestring criterion text
  ];
  homepage = "https://github.com/andrewthad/haskell-ip#readme";
  description = "Library for IP and MAC addresses";
  license = stdenv.lib.licenses.bsd3;
}
