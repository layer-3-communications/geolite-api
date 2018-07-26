{ mkDerivation, base, fetchgit, hashable, HUnit, stdenv, stm
, test-framework, test-framework-hunit
}:
mkDerivation {
  pname = "async";
  version = "2.2.1";
  src = fetchgit {
    url = "https://github.com/simonmar/async";
    sha256 = "1dp87srdrh4f4zfi7rp5krzvns0ppaqj7r84hcvyahgalmhlbkfk";
    rev = "b2a81599f19220263e7ac9a115750b09cab4fc53";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base hashable stm ];
  testHaskellDepends = [
    base HUnit stm test-framework test-framework-hunit
  ];
  homepage = "https://github.com/simonmar/async";
  description = "Run IO operations asynchronously and wait for their results";
  license = stdenv.lib.licenses.bsd3;
}
