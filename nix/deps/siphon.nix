{ mkDerivation, attoparsec, base, bytestring, colonnade
, contravariant, doctest, either, fetchgit, HUnit, pipes
, profunctors, QuickCheck, stdenv, streaming, test-framework
, test-framework-hunit, test-framework-quickcheck2, text
, transformers, vector
}:
mkDerivation {
  pname = "siphon";
  version = "0.8.1";
  src = fetchgit {
    url = "https://github.com/andrewthad/colonnade";
    sha256 = "089a3ryvcdyki34jkfm8fs467p0j4ym8pbi1av675gpibja5bvlj";
    rev = "06b5ffcd4096d5675d7dbe494de503e533bdfd59";
  };
  postUnpack = "sourceRoot+=/siphon; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    attoparsec base bytestring colonnade streaming text transformers
    vector
  ];
  testHaskellDepends = [
    base bytestring colonnade contravariant doctest either HUnit pipes
    profunctors QuickCheck streaming test-framework
    test-framework-hunit test-framework-quickcheck2 text
  ];
  homepage = "https://github.com/andrewthad/colonnade#readme";
  description = "Encode and decode CSV files";
  license = stdenv.lib.licenses.bsd3;
}
