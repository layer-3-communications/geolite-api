{ mkDerivation, attoparsec, base, base-compat, base-orphans
, base16-bytestring, bytestring, containers, deepseq, directory
, dlist, fetchgit, filepath, generic-deriving, ghc-prim, hashable
, hashable-time, integer-logarithms, QuickCheck
, quickcheck-instances, scientific, stdenv, tagged, tasty
, tasty-hunit, tasty-quickcheck, template-haskell, text
, th-abstraction, time, time-locale-compat, unordered-containers
, uuid-types, vector
}:
mkDerivation {
  pname = "aeson";
  version = "1.4.0.0";
  src = fetchgit {
    url = "https://github.com/bos/aeson.git";
    sha256 = "0i0rdsk5ls4s9xf27zi0imsikrjbmli8w14inaagzbx0wi2asg34";
    rev = "f7e380fc761896ab5ddd27a50456d109b15dae04";
  };
  libraryHaskellDepends = [
    attoparsec base base-compat bytestring containers deepseq dlist
    ghc-prim hashable scientific tagged template-haskell text
    th-abstraction time time-locale-compat unordered-containers
    uuid-types vector
  ];
  testHaskellDepends = [
    attoparsec base base-compat base-orphans base16-bytestring
    bytestring containers directory dlist filepath generic-deriving
    ghc-prim hashable hashable-time integer-logarithms QuickCheck
    quickcheck-instances scientific tagged tasty tasty-hunit
    tasty-quickcheck template-haskell text time time-locale-compat
    unordered-containers uuid-types vector
  ];
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}
