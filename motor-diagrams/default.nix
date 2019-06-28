{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865"
, motor, motor-reflection
}:

let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      inherit motor motor-reflection;
    };
  };
  # variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  drv = (haskellPackages.callCabal2nix "motor-diagrams" ./. {});
in
{
  motor-diagrams = drv;
}
