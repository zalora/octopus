{ pkgs ? import <nixpkgs> {}
, name ? "octopus"
, src ? builtins.filterSource (path: type: type != "unknown" && baseNameOf path != ".git") ./.
}:

{
  build = pkgs.haskellPackages.buildLocalCabal src name;
}
