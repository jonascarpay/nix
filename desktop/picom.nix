{ pkgs, lib, ... }:
{

  services.picom = {
    enable = true;
    experimentalBackends = true;

    # package = pkgs.picom.overrideAttrs (_: {
    #   src = pkgs.fetchFromGitHub {
    #     repo = "picom";
    #     owner = "yshui";
    #     rev = "3d358a06f2ef19d55f6cdca01dbfa51e112fcac7";
    #     sha256 = "1bjm9vl3dy4sbm4h2jvldjfpkx6rsrz7m81y9kikmf6ggl3a4x1z";
    #   };
    # });

    fade = true;
    fadeDelta = 4;

    shadow = true;
    noDockShadow = false;
    # shadowExclude = [ "!focused" ]; # breaks shit

    # blurExclude = [
    #   "class_i = 'Blender'"
    #   "class_i = 'gl'"
    # ];

    # opacityRule = [
    #   "100:class_i = 'Blender'"
    #   "100:class_i = 'gl'"
    # ];

    # inactiveOpacity = "0.9";
    # activeOpacity = "1";
    inactiveDim = "0.23";

    vSync = true;

    # extraOptions = ''
    #   blur = true;
    #   blur-method = "dual_kawase";
    #   blur-strength = 10;
    # '';
  };

}
