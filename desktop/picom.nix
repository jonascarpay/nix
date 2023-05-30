{
  services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
    shadow = true;
    inactiveOpacity = 0.85;
    settings = {
      blur = {
        method = "dual_kawase";
        blur-strength = 12;
      };
      blur-background-exclude = [ "class_i != 'st-256color'" ];
      inactive-dim = 0.20;
    };
    # shadowExclude = [ "class_g = 'i3-frame'" ];
    opacityRules = [ "100:class_i != 'st-256color'" ];
  };
}
