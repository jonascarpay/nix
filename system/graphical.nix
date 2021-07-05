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
    enable = true;
    displayManager.autoLogin = {
      enable = true;
      user = "jmc";
    };
    desktopManager.plasma5.enable = true; # TODO disable
  };
  networking.firewall = {
    allowedTCPPorts = [
      27037 # steam
      27036 # steam
    ];
    allowedUDPPorts = [
      8081 # steam, hoogle
      27036 # steam
      27031 # steam
    ];
  };
}
