{ mkDerivation, base, containers, contiguous, doctest, fetchgit
, gauge, ghc-prim, HUnit, primitive, QuickCheck, random, smallcheck
, stdenv, tasty, tasty-hunit, tasty-quickcheck, tasty-smallcheck
}:
mkDerivation {
  pname = "primitive-sort";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/primitive-sort.git";
    sha256 = "1mh9s041qaywgkz664c0h3g4w83sj4i6nx1qd2lf8q3a7ppwhp7p";
    rev = "0e30cf9eda0b29b4e66c56e093800e11063630b8";
  };
  libraryHaskellDepends = [ base contiguous ghc-prim primitive ];
  testHaskellDepends = [
    base containers doctest HUnit primitive QuickCheck smallcheck tasty
    tasty-hunit tasty-quickcheck tasty-smallcheck
  ];
  benchmarkHaskellDepends = [ base gauge ghc-prim primitive random ];
  homepage = "https://github.com/andrewthad/primitive-sort";
  description = "Sort primitive arrays";
  license = stdenv.lib.licenses.bsd3;
}
