{ mkDerivation, base, containers, fetchgit, stdenv }:
mkDerivation {
  pname = "diffmap";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/chessai/diffmap.git";
    sha256 = "1flv8r8mkvzndwpifv7nf7zll1j3w24gzlh7qnx7mnbfaqhwqva5";
    rev = "eb9ca991fdb99b7bb3fcc5275dff6ee1349edc1e";
  };
  libraryHaskellDepends = [ base containers ];
  homepage = "https://github.com/chessai/diffmap.git";
  description = "diff on maps";
  license = stdenv.lib.licenses.bsd3;
}
