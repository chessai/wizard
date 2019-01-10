{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  wizard = (
    with rec {
      wizardSource = pkgs.lib.cleanSource ../.;
      wizardBasic  = self.callCabal2nix "wizard" wizardSource { };
    };
    overrideCabal wizardBasic (old: {
    })
  );
}
