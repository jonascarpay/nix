{ pkgs, unstable, inputs, lib, ... }:
let

  # caches =
  #   let
  #     cachix = [{ name = "nix-community"; sha256 = "sha256:0m6kb0a0m3pr6bbzqz54x37h5ri121sraj1idfmsrr6prknc7q3x"; }];
  #     # caches = [
  #     #   { url = "https://hydra.iohk.io"; key = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="; }
  #     # ];
  #   in
  #   {
  #     inherit cachix;
  #     # nix.binaryCaches = builtins.map (a: a.url) caches;
  #     # nix.binaryCachePublicKeys = builtins.map (a: a.key) caches;
  #   };

  # boot-switch565 =
  #   let
  #     archive = pkgs.fetchzip {
  #       url = "https://github.com/CTCaer/hekate/releases/download/v5.6.5/hekate_ctcaer_5.6.5_Nyx_1.1.1.zip";
  #       sha256 = "sha256-ycXyddzcKzjFZFPhs3OrdMvPWZBOvuhLoSssQyQ9nss=";
  #       stripRoot = false;
  #     };
  #   in
  #   pkgs.writeShellScriptBin "boot-switch565" ''
  #     ${pkgs.fusee-launcher}/bin/fusee-launcher ${archive}/hekate_ctcaer_5.6.5.bin
  #   '';

  # boot-switch602 =
  #   let
  #     archive = pkgs.fetchzip {
  #       url = "https://github.com/CTCaer/hekate/releases/download/v6.0.2/hekate_ctcaer_6.0.2_Nyx_1.5.2.zip";
  #       sha256 = "sha256-I7WYxBIgBQjqwNlwIv6RRJ0LXTk5E3PsOC6c8/nJNxA=";
  #       stripRoot = false;
  #     };
  #   in
  #   pkgs.writeShellScriptBin "boot-switch602" ''
  #     ${pkgs.fusee-launcher}/bin/fusee-launcher ${archive}/hekate_ctcaer_6.0.2.bin
  #   '';

  scrubnix = pkgs.writeShellScriptBin "scrubnix" ''
    sed -E 's#/nix/store/([a-z0-9]{5})[a-z0-9]{27}-([a-zA-Z0-9+?=._-]+)#<\2_\1>#g' -
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

in
{
  imports = [
    home-manager
    agenix
    words-txt
    # caches
    # notifications-token
  ];
  environment = {
    systemPackages = with pkgs; [
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
      # scrubnix
      # tagref
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
    pathsToLink = [ "/share/zsh" "/share/fish" ];
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
  # services.mullvad-vpn.enableExcludeWrapper = true;

  programs.bash.enableCompletion = true; # enable tab-completion for nix-* tools
  programs.mosh.enable = true;
  programs.git.enable = true;

  programs.command-not-found.enable = false;
  hardware.enableRedistributableFirmware = true;

  users = {
    mutableUsers = false;
    users.jmc = {
      isNormalUser = true;
      description = "Jonas Carpay";
      createHome = true;
      extraGroups = [ "wheel" "networkmanager" "audio" ];
      # TODO ed
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDOlH/zVT9nxSmWA2xcenBhHg0+21WtYV3ZugYWopq1CKdHuPKcgbJ5mq3yvQaWEDJu/bxeuj98F2jJVALJRuVk215GQQxBZlUn3HQefEO5wxpL9lb64B/ElBcoMLBbD3PqdXGvd2QEwOnJvDB+AJY5AtgjEdAvxgfeta/Yd4bmKlSC2Gq6UbpM3zAfN4gH1wRnUm22hTpEsyPVygfu0vfwSY54/DtAXoK7t/6WseLEcIfO/1KD0KZRH+7KDXh3prAV4MwtrNItLGUXDoUzUbF+da/5NHQG+bQrNQiubQJzoGPvo7ecDfvwLDQwBbzS2+OIBQZhP171cQyTmPU0xzwts2cF8XIcJpLVdmU36zHUXxCQIBDzxqAqjLGeh8DbK/e04tlmtfMw0xygUyXI9rXxOFoIy3c5DTDfveYj4G22W2WG+3zX1LznFYNXQLjlsiEheBzt0utC4zbrL7lc0SEihni4yajeWOcCElBBI57NDspI2HtgVXPI15NzbuNM1WCiz6Hq9xtTgums5n2K6bIKFvvqvhMBbJctBFqgthG3j24J/ajK6ZhCl5aqsxr6tRKr0M5qvAGnu0cNxAhmVkx2L7gOopxccVRuPx91moDOvPb185SWf/zL0NSkiSNhuO2lMZ9XjxMOf+VShSvEPu8mTksy0GumhxEjmXx4yhhAnw== jonascarpay@gmail.com"
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
  # programs.dconf.enable = true; # somehow necessary for home-manager to switch configurations?
}

# vim: nowrap
