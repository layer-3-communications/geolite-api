{ mkDerivation, base, bytestring, colonnade, containers
, disjoint-containers, fetchgit, siphon, stdenv, streaming
, streaming-bytestring, text, transformers
}:
mkDerivation {
  pname = "country-code-generation";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/country.git";
    sha256 = "0xs8mp6snwd22g91cqjby7lhcd9b815z3md8fzpz2y8kplslhxra";
    rev = "fde6d11925efb48f7b2d564ebedb5e1fbf03af50";
  };
  postUnpack = "sourceRoot+=/country-code-generation; echo source root reset to $sourceRoot";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring colonnade containers disjoint-containers siphon
    streaming streaming-bytestring text transformers
  ];
  homepage = "https://github.com/andrewthad/country#readme";
  license = stdenv.lib.licenses.bsd3;
}
