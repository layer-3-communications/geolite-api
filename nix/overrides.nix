{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  ip = dontCheck (self.callPackage ./deps/ip.nix {}); 
   
  siphon = dontCheck super.siphon;

  contiguous = ( self.callPackage ./deps/contiguous.nix {} );

  primitive = super.primitive_0_6_4_0;
  
  quantification = self.callPackage ./deps/quantification.nix {};

  primitive-containers =
    doJailbreak
      ( dontBenchmark
        ( dontHaddock
          ( dontCheck
            ( self.callPackage ./deps/primitive-containers.nix {}
            )
          )
        )
      ); 
  
  geolite-api = (
    with rec {
      geolite-apiSource = pkgs.lib.cleanSource ../.;
      geolite-apiBasic = self.callCabal2nix "geolite-api" geolite-apiSource {};
    };
    overrideCabal geolite-apiBasic (old: {
    })
  );
}
