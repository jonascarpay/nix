{ pkgs, unstable, inputs, lib, ... }:
let

  scrubnix = pkgs.writeShellScriptBin "scrubnix" ''
    if [[ $* == *-v* ]]; then
      sed --unbuffered -E 's#/nix/store/([a-z0-9]{5})[a-z0-9]{27}-([a-zA-Z0-9+?=._-]+)#<\2@\1>#g' -
    else
      sed --unbuffered -E 's#/nix/store/[a-z0-9]{32}-([a-zA-Z0-9+?=._-]+)#<\1>#g' -
    fi
  '';

  home-manager = {
    imports = [ inputs.home-manager.nixosModules.home-manager ];
    home-manager = {
      # Currently necessary when using HM as a flake
      # https://nix-community.github.io/home-manager/index.html#sec-flakes-nixos-module
      # https://github.com/divnix/digga/issues/30#issuecomment-748530996
      useGlobalPkgs = true;
      useUserPackages = true;
      users.jmc.imports = [ ../home ];
      extraSpecialArgs = { inherit unstable inputs; };
    };
  };

  fish = {
    users.users.jmc.shell = pkgs.fish;
    programs.fish.enable = true;
  };

  agenix = {
    imports = [ inputs.agenix.nixosModules.age ];
    environment.systemPackages = [ inputs.agenix.packages.${pkgs.system}.agenix ];
  };

  words-txt = {
    environment.etc."words.txt".source =
      let
        src = pkgs.fetchFromGitHub {
          owner = "dwyl";
          repo = "english-words";
          rev = "11735d0d68f051b817ad224e14d999acc94fcf00";
          sha256 = "sha256-xdCsKX04Zc2mtUTic+WDtBL+02lFYzHne7gm7pXd50o=";
        };
      in
      "${src}/words.txt";
  };

  nix-index-database = {
    imports = [ inputs.nix-index-database.nixosModules.nix-index ];
    programs.nix-index-database.comma.enable = true;
  };
in
{
  imports = [
    home-manager
    agenix
    words-txt
    fish
    # caches
    # notifications-token
    nix-index-database
  ];
  environment = {
    systemPackages = with pkgs; [
      ascii
      dnsutils
      exfat
      file
      gotop
      htop
      ncdu
      # myNnn
      p7zip
      ranger
      sshfs
      scrubnix
      libqalculate
      tmux
      # boot-switch565
      # boot-switch602
      tree
      # nix-tree
      # btop
      unzip
      # viddy
      # nix-output-monitor
      vim
    ] ++ lib.optional (system == "x86_64-linux") lnav; # https://github.com/tstack/lnav/issues/882
    variables = {
      EDITOR = "vim";
      PAGER = "less";
    };

    # zsh needs this for autoCompletion according to home-configuration
    # for fish, direnv suddenly stopped working, home-configuration suggested adding this and that seemed to have fixed it.
    # TODO occasionally see if I can safely remove this
    pathsToLink = [
      "/share/zsh"
      "/share/fish"
    ];
  };

  nixpkgs.config.allowUnfree = true; # TODO move to flake cfg?

  security.sudo.wheelNeedsPassword = false;
  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
    settings.UseDns = true;
  };
  services.tuptime.enable = true;
  # services.mullvad-vpn.enable = true;

  programs.bash.completion.enable = true; # enable tab-completion for nix-* tools
  programs.mosh.enable = true;
  programs.git.enable = true;

  hardware.enableRedistributableFirmware = true;

  users = {
    mutableUsers = false;
    users.jmc = {
      isNormalUser = true;
      description = "Jonas Carpay";
      createHome = true;
      extraGroups = [
        "wheel"
        "networkmanager"
        "audio"
        "tty"
        "dialout"
      ];
      # TODO ed
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDOlH/zVT9nxSmWA2xcenBhHg0+21WtYV3ZugYWopq1CKdHuPKcgbJ5mq3yvQaWEDJu/bxeuj98F2jJVALJRuVk215GQQxBZlUn3HQefEO5wxpL9lb64B/ElBcoMLBbD3PqdXGvd2QEwOnJvDB+AJY5AtgjEdAvxgfeta/Yd4bmKlSC2Gq6UbpM3zAfN4gH1wRnUm22hTpEsyPVygfu0vfwSY54/DtAXoK7t/6WseLEcIfO/1KD0KZRH+7KDXh3prAV4MwtrNItLGUXDoUzUbF+da/5NHQG+bQrNQiubQJzoGPvo7ecDfvwLDQwBbzS2+OIBQZhP171cQyTmPU0xzwts2cF8XIcJpLVdmU36zHUXxCQIBDzxqAqjLGeh8DbK/e04tlmtfMw0xygUyXI9rXxOFoIy3c5DTDfveYj4G22W2WG+3zX1LznFYNXQLjlsiEheBzt0utC4zbrL7lc0SEihni4yajeWOcCElBBI57NDspI2HtgVXPI15NzbuNM1WCiz6Hq9xtTgums5n2K6bIKFvvqvhMBbJctBFqgthG3j24J/ajK6ZhCl5aqsxr6tRKr0M5qvAGnu0cNxAhmVkx2L7gOopxccVRuPx91moDOvPb185SWf/zL0NSkiSNhuO2lMZ9XjxMOf+VShSvEPu8mTksy0GumhxEjmXx4yhhAnw== jonascarpay@gmail.com"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJog1Go1KtITOMM1SNk1yqZ+S7F03/+WGd8H6mYBra8M ipro"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILyj3zPZyLZp/VgNwem7rBhqYr0iVSJsZGIEZ/HU/kMw tcl@cedbook.local"
      ];
      # mkpasswd -m sha-512
      hashedPassword = "$6$YfPTTLclMnT7Pz.9$7UkmuSo9QHs4rhWDZ.8RRwGNRgkc6Nx8/L11A9.wcWBwuCdQyCgjZj0BB6Kc2AavDAtlT125o5wNlvzv2mrcJ0";
    };
    users.root.hashedPassword = "$6$m26D1J9rYXRcv.UX$sMAEzl.q5Bj2GrvJ1S5t09rCS499dQeuL08blpLl3ZdjTNKoD5a7.SkQUZwVMkF5Ui8GWoR26O2kdSdRnyPfK1";
  };

  documentation = {
    nixos.enable = true;
    # nixos.includeAllModules = true;
    man.generateCaches = true; # Needed for fish man page completions
    dev.enable = true;
  };

  boot.tmp.cleanOnBoot = true;

  nix = {
    settings.experimental-features = [ "nix-command" "flakes" ];
    # package = pkgs.nixUnstable;
    # https://github.com/gytis-ivaskevicius/flake-utils-plus/blob/master/modules/saneFlakeDefaults.nix
    registry =
      builtins.mapAttrs
        (name: v: { flake = v; })
        (lib.filterAttrs
          (name: value: value ? outputs)
          inputs);
  };
}

# vim: nowrap
