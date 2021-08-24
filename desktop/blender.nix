{ pkgs, unstable, config, ... }:
let

  blender3 =
    let
      args.cudaSupport = true;
      attrs = old: {
        version = "2021-07-19";
        src = pkgs.fetchFromGitHub {
          owner = "blender";
          repo = "blender";
          rev = "ddecd7aaca880586c2762c5a85d90ee8b44cd0a1";
          sha256 = "sha256-gv3IjeEVEKNSCqH2nXcfBMA6zm0UR2EM9xK568X1woo=";
          fetchSubmodules = true;
        };
      };
    in
    (unstable.blender.override args).overrideAttrs attrs;

in
{
  home.packages = [
    blender3
    unstable.aseprite
  ];
}
