{ mkDerivation, base, fetchgit, primitive, stdenv }:
mkDerivation {
  pname = "contiguous";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/contiguous.git";
    sha256 = "0q413s7bsq3fdjlr1mif1jfvkrwc92cn5wqhds1ngnf8rkqw60vw";
    rev = "89a653dde24076f1394f6e69d061fc52ab1b105f";
  };
  libraryHaskellDepends = [ base primitive ];
  homepage = "https://github.com/andrewthad/contiguous";
  description = "Unified interface for primitive arrays";
  license = stdenv.lib.licenses.bsd3;
}
