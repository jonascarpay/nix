{ pkgs, lib, config, ... }: {
  services.picom = {
    enable = true;
    package = pkgs.picom.overrideAttrs (_: {
      version = "2021-07-14";
      src = pkgs.fetchFromGitHub {
        repo = "picom";
        owner = "yshui";
        rev = "057a939431a5e856d001a5b2be9b9ba86738e829";
        sha256 = "01apbg9z4195623j5pi2xrc0aqaffkxpkrf8j53rbyi5xlg9i5gx";
      };
    });
    vSync = true;
    shadow = true;
    blur = true;
    experimentalBackends = true;
    noDockShadow = false;
    fade = false;
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
}
