{ system ? builtins.currentSystem
, pkgs ? import <nixpkgs> { inherit system; }
, name ? "octopus"
, haskellPackages ? pkgs.haskellPackages
, src ? builtins.filterSource (path: type: type != "unknown" && baseNameOf path != ".git" && baseNameOf path != "result") ./.
}:

{
  build = haskellPackages.buildLocalCabal src name;
}
