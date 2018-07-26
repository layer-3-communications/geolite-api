{ mkDerivation, aeson, attoparsec, base, bytestring, criterion
, doctest, fetchgit, hashable, hspec, HUnit, primitive, QuickCheck
, quickcheck-classes, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, vector
}:
mkDerivation {
  pname = "ip";
  version = "1.4.0";
  src = fetchgit {
    url = "http://github.com/andrewthad/haskell-ip.git";
    sha256 = "1vyyhnscipmsdinngj3qspaxdgp23rqvjfz34ag9f36c19lidvrf";
    rev = "235aa24e9450c81f93cbe989986f9aa5b6874dc6";
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring hashable primitive text vector
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
