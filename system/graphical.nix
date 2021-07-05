{
  hardware = {
    pulseaudio.enable = true;
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
  };
  services.xserver = {
    libinput.enable = true;
    enable = true;
    displayManager.autoLogin = {
      enable = true;
      user = "jmc";
    };
    desktopManager.plasma5.enable = true; # TODO disable
  };
}
