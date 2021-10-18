{ pkgs, unstable, inputs, lib, ... }:
let

  caches =
    let
      cachix = [
        { name = "jmc"; sha256 = "1bk08lvxi41ppvry72y1b9fi7bb6qvsw6bn1ifzsn46s3j0idq0a"; }
        { name = "nix-community"; sha256 = "1r0dsyhypwqgw3i5c2rd5njay8gqw9hijiahbc2jvf0h52viyd9i"; }
      ];
      caches = [
        { url = "https://hydra.iohk.io"; key = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="; }
      ];
    in
    {
      inherit cachix;
      nix.binaryCaches = builtins.map (a: a.url) caches;
      nix.binaryCachePublicKeys = builtins.map (a: a.key) caches;
    };

  myNnn =
    let nnn = pkgs.nnn.override { withNerdIcons = true; };
    in
    pkgs.writeShellScriptBin "nnn" ''
      ${nnn}/bin/nnn -e $@
    '';

in
{
  imports = [
    caches
    ./xc-cache.nix
  ];
  environment = {
    systemPackages = with pkgs; [
      dnsutils
      exfat
      file
      git
      gotop
      htop
      ncdu
      myNnn
      p7zip
      ranger
      sshfs
      tmux
      tree
      unstable.nix-tree
      unstable.btop
      unzip
      vim
    ] ++ lib.optional (system == "x86_64-linux") lnav; # https://github.com/tstack/lnav/issues/882
    variables = {
      EDITOR = "vim";
      PAGER = "less";
    };
    homeBinInPath = true;
    # required for zsh autoCompletion according to home-configuration.nix
    pathsToLink = [ "/share/zsh" ];
    etc."words.txt".source =
      let
        src = pkgs.fetchFromGitHub {
          owner = "dwyl";
          repo = "english-words";
          rev = "11735d0d68f051b817ad224e14d999acc94fcf00";
          sha256 = "sha256-xdCsKX04Zc2mtUTic+WDtBL+02lFYzHne7gm7pXd50o=";
        };
      in
      "${src}/words_alpha.txt";
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
    useDns = true;
  };
  programs.bash.enableCompletion = true; # enable tab-completion for nix-* tools

  hardware.enableRedistributableFirmware = true;

  users = {
    mutableUsers = false;
    users.jmc = {
      isNormalUser = true;
      description = "Jonas Carpay";
      createHome = true;
      extraGroups = [ "wheel" "networkmanager" "audio" "syncthing" "transmission" ];
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDOlH/zVT9nxSmWA2xcenBhHg0+21WtYV3ZugYWopq1CKdHuPKcgbJ5mq3yvQaWEDJu/bxeuj98F2jJVALJRuVk215GQQxBZlUn3HQefEO5wxpL9lb64B/ElBcoMLBbD3PqdXGvd2QEwOnJvDB+AJY5AtgjEdAvxgfeta/Yd4bmKlSC2Gq6UbpM3zAfN4gH1wRnUm22hTpEsyPVygfu0vfwSY54/DtAXoK7t/6WseLEcIfO/1KD0KZRH+7KDXh3prAV4MwtrNItLGUXDoUzUbF+da/5NHQG+bQrNQiubQJzoGPvo7ecDfvwLDQwBbzS2+OIBQZhP171cQyTmPU0xzwts2cF8XIcJpLVdmU36zHUXxCQIBDzxqAqjLGeh8DbK/e04tlmtfMw0xygUyXI9rXxOFoIy3c5DTDfveYj4G22W2WG+3zX1LznFYNXQLjlsiEheBzt0utC4zbrL7lc0SEihni4yajeWOcCElBBI57NDspI2HtgVXPI15NzbuNM1WCiz6Hq9xtTgums5n2K6bIKFvvqvhMBbJctBFqgthG3j24J/ajK6ZhCl5aqsxr6tRKr0M5qvAGnu0cNxAhmVkx2L7gOopxccVRuPx91moDOvPb185SWf/zL0NSkiSNhuO2lMZ9XjxMOf+VShSvEPu8mTksy0GumhxEjmXx4yhhAnw== jonascarpay@gmail.com"
      ];
      hashedPassword = "$1$Nh9Zc.7Y$Zw9...mvYbA0qWE/PXKm7.";
    };
  };
  documentation = {
    nixos.includeAllModules = true;
    man.generateCaches = true;
    dev.enable = true;
  };

  nix = {
    # trustedUsers = [ "root" "@wheel" ];
    package = pkgs.nixUnstable;
    extraOptions = "experimental-features = nix-command flakes";
    # https://github.com/gytis-ivaskevicius/flake-utils-plus/blob/master/modules/saneFlakeDefaults.nix
    registry =
      builtins.mapAttrs
        (name: v: { flake = v; })
        (lib.filterAttrs
          (name: value: value ? outputs)
          inputs);
  };

}
