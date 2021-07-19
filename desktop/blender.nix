{ pkgs, config, ... }:
let
  unstable = pkgs.unstable;

  blender3 =
    let
      args = {
        cudaSupport = true;
      };
      attrs = old: {
        version = "2021-07-19";
        src = pkgs.fetchFromGitHub {
          owner = "blender";
          repo = "blender";
          rev = "ff01070b5ca98a3019a66526c8c26c1f64bb0ef4";
          sha256 = "1j8sg7q6ks2gsx45ypbk30a6155xij1l6d5bjl30hzqfwsibpy79";

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
