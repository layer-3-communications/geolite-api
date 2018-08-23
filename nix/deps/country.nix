{ mkDerivation, aeson, attoparsec, base, bytestring, deepseq
, fetchgit, ghc-prim, hashable, primitive, scientific, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "country";
  version = "0.1.6";
  src = fetchgit {
    url = "https://github.com/andrewthad/country";
    sha256 = "0j2y6azbl1c5b2i3b8vlc6x7cmk9fkbna1b4sw5b85sqmaqqgjcr";
    rev = "18cabf91a052d575485dfa54de8ba104e6840173";
  };
  postUnpack = "sourceRoot+=/country; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec base bytestring deepseq ghc-prim hashable
    primitive scientific text unordered-containers
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/andrewthad/country#readme";
  description = "Country data type and functions";
  license = stdenv.lib.licenses.bsd3;
}
