{ pkgs, ... }:
{
  imports = [ ./xc-cache.nix ];
  environment = {
    systemPackages = with pkgs; [
      dnsutils
      exfat
      file
      git
      gotop
      htop
      ncdu
      ranger
      sshfs
      tmux
      tree
      vim
    ];
    variables = {
      EDITOR = "vim";
      PAGER = "less";
    };
    homeBinInPath = true;
    # required for zsh autoCompletion according to home-configuration.nix
    pathsToLink = [ "/share/zsh" ];
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22
      80
      8080
      8081
      22000 # syncthing
    ];
    allowedUDPPorts = [
      21027 # syncthing
    ];
  };

  nixpkgs.config.allowUnfree = true;
  security.sudo.wheelNeedsPassword = false;
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  programs.bash.enableCompletion = true; # enable tab-completion for nix-* tools

  hardware.enableRedistributableFirmware = true;

  users = {
    mutableUsers = false;
    users.jmc = {
      isNormalUser = true;
      description = "Jonas Carpay";
      createHome = true;
      extraGroups = [ "wheel" "networkmanager" "audio" ];
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDOlH/zVT9nxSmWA2xcenBhHg0+21WtYV3ZugYWopq1CKdHuPKcgbJ5mq3yvQaWEDJu/bxeuj98F2jJVALJRuVk215GQQxBZlUn3HQefEO5wxpL9lb64B/ElBcoMLBbD3PqdXGvd2QEwOnJvDB+AJY5AtgjEdAvxgfeta/Yd4bmKlSC2Gq6UbpM3zAfN4gH1wRnUm22hTpEsyPVygfu0vfwSY54/DtAXoK7t/6WseLEcIfO/1KD0KZRH+7KDXh3prAV4MwtrNItLGUXDoUzUbF+da/5NHQG+bQrNQiubQJzoGPvo7ecDfvwLDQwBbzS2+OIBQZhP171cQyTmPU0xzwts2cF8XIcJpLVdmU36zHUXxCQIBDzxqAqjLGeh8DbK/e04tlmtfMw0xygUyXI9rXxOFoIy3c5DTDfveYj4G22W2WG+3zX1LznFYNXQLjlsiEheBzt0utC4zbrL7lc0SEihni4yajeWOcCElBBI57NDspI2HtgVXPI15NzbuNM1WCiz6Hq9xtTgums5n2K6bIKFvvqvhMBbJctBFqgthG3j24J/ajK6ZhCl5aqsxr6tRKr0M5qvAGnu0cNxAhmVkx2L7gOopxccVRuPx91moDOvPb185SWf/zL0NSkiSNhuO2lMZ9XjxMOf+VShSvEPu8mTksy0GumhxEjmXx4yhhAnw== jonascarpay@gmail.com"
      ];
      hashedPassword = "$1$Nh9Zc.7Y$Zw9...mvYbA0qWE/PXKm7.";
    };
  };

  nix = {
    trustedUsers = [ "root" "@wheel" ];
    package = pkgs.nixUnstable;
    extraOptions = "experimental-features = nix-command flakes";
  };

}
