{ pkgs, lib, config, ... }: {
  services.picom = {
    enable = true;
    package = pkgs.picom.overrideAttrs (_: {
      src = pkgs.fetchFromGitHub {
        repo = "picom";
        owner = "yshui";
        rev = "5388ba0946bb325b343e04c4dfc66ca05d4d1466";
        sha256 = "sha256-B3lpZPMLwiGLmOwJ3DAHDfgtv9cMRYPNpKnqmItbKlM=";
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
