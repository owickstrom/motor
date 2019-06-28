{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865"
, motor
}:

let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      inherit motor;
    };
  };
  # variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = (haskellPackages.callCabal2nix "motor-reflection" ./. {});
in
{
  motor-reflection = drv;
}
