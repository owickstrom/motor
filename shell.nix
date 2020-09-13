{ pkgs ? import <nixpkgs> {}, compiler ? "ghc8102", doBenchmark ? true }:
let
  haskellPackages = pkgs.haskell.packages.${compiler};
  project = import ./. { inherit compiler doBenchmark; };
in
  haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [
      project.motor
      project.motor-reflection
      project.motor-diagrams
    ];
    buildInputs = with pkgs; [
      plantuml
    ];
  }
