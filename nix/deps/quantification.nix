{ mkDerivation, aeson, base, containers, fetchgit, ghc-prim
, hashable, path-pieces, stdenv, text, unordered-containers, vector
}:
mkDerivation {
  pname = "quantification";
  version = "0.5.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/quantification.git";
    sha256 = "1zgn6f3bsd3ra9ykkkbm75lai77x31mqg7w7lphdxiv901415h5s";
    rev = "72aaaa49dd1870804ca22a70955585c68af7eeac";
  };
  libraryHaskellDepends = [
    aeson base containers ghc-prim hashable path-pieces text
    unordered-containers vector
  ];
  homepage = "https://github.com/andrewthad/quantification#readme";
  description = "Rage against the quantification";
  license = stdenv.lib.licenses.bsd3;
}
