{ compiler ? "ghc843" }:

with rec {
  fetchFromGitHub = (
    (import <nixpkgs> { config = { allowUnfree = true; }; overlays = []; }).fetchFromGitHub);
  _nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "8d630b308752dbdb8b746d2ae986b7c388dd8ac3";
    sha256 = "0hp9zqbjjf4vyqxg62dc0yxwm2wmam71fhvam61b5j7als9n8c84"; 
  };
};

import _nixpkgs {
  config = {
    allowUnfree = true;
    packageOverrides = super: let self = super.pkgs; in {
      haskellPackages = super.haskell.packages.${compiler}.override {
        overrides = import ./overrides.nix { pkgs = self; };
      };

    };
  };
  overlays = [];
}
