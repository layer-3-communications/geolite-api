{ mkDerivation, base, containers, fetchgit, semigroupoids, stdenv
}:
mkDerivation {
  pname = "non-empty-containers";
  version = "0.1.1.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/non-empty-containers.git";
    sha256 = "17icv04mpd2clamq1r0b74zr47gcpw610xxgkkbsvsax1cnafp96";
    rev = "a8293c40382983622cea9703da7dc841093e016c";
  };
  libraryHaskellDepends = [ base containers semigroupoids ];
  homepage = "https://github.com/andrewthad/non-empty-containers#readme";
  license = stdenv.lib.licenses.bsd3;
}
