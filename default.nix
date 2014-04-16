{ pkgs ? import <nixpkgs> {}
, name ? "octopus"
, haskellPackages ? pkgs.haskellPackages
, src ? builtins.filterSource (path: type: type != "unknown" && baseNameOf path != ".git") ./.
}:

{
  build = haskellPackages.buildLocalCabal src name;
}
