{ package ? "discrete-intervals", compiler ? "ghc841" }:

(import ./default.nix {
  inherit package compiler;
}).discrete-intervals
