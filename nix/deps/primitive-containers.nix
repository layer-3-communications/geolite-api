{ mkDerivation, aeson, base, containers, contiguous, fetchgit
, gauge, ghc-prim, HUnit, primitive, primitive-sort, quantification
, QuickCheck, quickcheck-classes, random, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, unordered-containers, vector
}:
mkDerivation {
  pname = "primitive-containers";
  version = "0.3.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/primitive-containers.git";
    sha256 = "0a7n5gylsvpj4zmbzy4v3h41pkbz6m0iylf7axq89wcycx342n9i";
    rev = "0a04809ee98af2aa26af134179092faae54ea752";
  };
  libraryHaskellDepends = [
    aeson base contiguous primitive primitive-sort quantification text
    unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base containers HUnit primitive quantification QuickCheck
    quickcheck-classes tasty tasty-hunit tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [
    base containers gauge ghc-prim primitive random
  ];
  homepage = "https://github.com/andrewthad/primitive-containers";
  description = "containers backed by arrays";
  license = stdenv.lib.licenses.bsd3;
}
