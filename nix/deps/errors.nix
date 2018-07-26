{ mkDerivation, base, exceptions, fetchgit, safe, stdenv, text
, transformers, transformers-compat
}:
mkDerivation {
  pname = "errors";
  version = "2.3.0";
  src = fetchgit {
    url = "https://github.com/Gabriel439/Haskell-Errors-Library.git";
    sha256 = "0b11lcfxjzll8s79ydn8x4qmhdy0a7y76r5c38j5k75d92qw6z70";
    rev = "cdf6943280f48dd9bf3faed295fe0626ffbdead8";
  };
  libraryHaskellDepends = [
    base exceptions safe text transformers transformers-compat
  ];
  description = "Simplified error-handling";
  license = stdenv.lib.licenses.bsd3;
}
