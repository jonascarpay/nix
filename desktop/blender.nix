{ pkgs, config, ... }:
let

  unstable = config.channels.unstable;

  blender3 =
    let
      args = {
        cudaSupport = true;
      };
      attrs = old: {
        version = "3.0.0";
        src = pkgs.fetchFromGitHub {
          owner = "blender";
          repo = "blender";
          rev = "5759bbe9f9e8706356c9d39939f472e3bb717860";
          sha256 = "0x4yk2zf07msm2x6ic3l5ccahvrz41mrim89vcbvwc4wfmrr2qwz";
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
