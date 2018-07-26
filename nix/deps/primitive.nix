{ mkDerivation, base, fetchgit, ghc-prim, stdenv, transformers }:
mkDerivation {
  pname = "primitive";
  version = "0.6.4.0";
  src = fetchgit {
    url = "https://github.com/haskell/primitive.git";
    sha256 = "1nh4qrvhkp281lw2dlw2dpqca2vm523agfha3bs4h1k89zykjsqr";
    rev = "ea9f0426646ecf8a5a0a1659b29a8dc014ceda05";
  };
  libraryHaskellDepends = [ base ghc-prim transformers ];
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = stdenv.lib.licenses.bsd3;
}
