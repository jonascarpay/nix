{ pkgs, inputs, ... }: {
  services.picom = {
    enable = true;
    package = pkgs.picom.overrideAttrs (_: { src = inputs.picom; });
    vSync = true;
    shadow = true;
    blur = true;
    noDockShadow = false;
    fade = false;
    fadeDelta = 4;
    extraOptions = ''
      blur-method = "dual_kawase";
      blur-strength = 12;
    '';
    blurExclude = [ "class_i != 'st-256color'" ];
    shadowExclude = [ "class_g = 'i3-frame'" ];
    inactiveOpacity = "0.85";
    activeOpacity = "1";
    inactiveDim = "0.20";
    opacityRule = [ "100:class_i != 'st-256color'" ];
  };
}
