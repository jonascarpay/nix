{ pkgs, lib, config, ... }:
let
  base = {
    enable = true;
    package = pkgs.picom.overrideAttrs (_: {
      src = pkgs.fetchFromGitHub {
        repo = "picom";
        owner = "yshui";
        rev = "d974367a0446f4f1939daaada7cb6bca84c893ef";
        sha256 = "0mjm544vck493sdcvr90d9ycg5sxin28a13w61q1kqkycilv87lv";
      };
    });
    vSync = true;
    shadow = true;
  };

  shadowFocus = {
    shadowExclude = [ "!focused" ]; # breaks shit
    shadowOffsets = [ 0 0 ];
    inactiveDim = "0.30";
  };

  blur = {
    blur = false;
    experimentalBackends = true;
    noDockShadow = false;
    fade = true;
    fadeDelta = 4;
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 12;
    '';
    blurExclude = [ "class_i != 'st-256color'" ];
    inactiveOpacity = "0.85";
    activeOpacity = "1";
    inactiveDim = "0.20";
    opacityRule = [ "100:class_i != 'st-256color'" ];
  };

in
# { services.picom = base // shadowFocus; }
{ services.picom = base // blur; }
