{ mkDerivation, aeson, base, bifunctors, containers, fetchgit
, primitive, QuickCheck, semigroupoids, semigroups, semirings
, stdenv, tagged, transformers, vector
}:
mkDerivation {
  pname = "quickcheck-classes";
  version = "0.4.12";
  src = fetchgit {
    url = "https://github.com/andrewthad/quickcheck-classes.git";
    sha256 = "17v2rv5sv4gp96nv74xdh4qb1422wrm7h5fyj8azkqvaasws00s6";
    rev = "c11b42f282679cbd3d3c26874505da810308c528";
  };
  libraryHaskellDepends = [
    aeson base bifunctors containers primitive QuickCheck semigroupoids
    semigroups semirings tagged transformers
  ];
  testHaskellDepends = [
    aeson base containers primitive QuickCheck semigroupoids tagged
    transformers vector
  ];
  homepage = "https://github.com/andrewthad/quickcheck-classes#readme";
  description = "QuickCheck common typeclasses";
  license = stdenv.lib.licenses.bsd3;
}
