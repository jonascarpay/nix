{ pkgs, config, ... }:
let
  unstable = pkgs.unstable;

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
          rev = "22c4323b2169e700fa124b3dfe56b750bc532e49";
          sha256 = "sha256-Z0AknIAd9LKCV2gBvDQhktJA+Mjz/OVAKmZkQRe5oCw=";

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
