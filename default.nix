{ pkgs ? import <nixpkgs> {}, compiler ? "ghc8102", doBenchmark ? false }:
rec {
  motor = (import ./motor { inherit compiler; }).motor;
  motor-reflection = (import ./motor-reflection { inherit compiler motor; }).motor-reflection;
  motor-diagrams = (import ./motor-diagrams { inherit compiler motor motor-reflection; }).motor-diagrams;
}
