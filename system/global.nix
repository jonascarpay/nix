{ pkgs, inputs, lib, ... }:
let

  caches =
    let
      cachix = [{ name = "nix-community"; sha256 = "sha256:1rgbl9hzmpi5x2xx9777sf6jamz5b9qg72hkdn1vnhyqcy008xwg"; }];
      # caches = [
      #   { url = "https://hydra.iohk.io"; key = "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="; }
      # ];
    in
    {
      inherit cachix;
      # nix.binaryCaches = builtins.map (a: a.url) caches;
      # nix.binaryCachePublicKeys = builtins.map (a: a.key) caches;
    };

  myNnn =
    let nnn = pkgs.nnn.override { withNerdIcons = true; };
    in
    pkgs.writeShellScriptBin "nnn" ''
      ${nnn}/bin/nnn -e $@
    '';

  # This is a hack to keep my notifications token secret.
  # If agenix gets home-manager support it can be moved to the polybar module.
  notifications-token = {
    age.secrets.notifications-token.file = ../secrets/notifications-token.age;
    age.secrets.notifications-token.owner = "jmc";
  };

in
{
  imports = [
    caches
    ./xc-cache.nix
    notifications-token
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
      nix-tree
      btop
      unzip
      viddy
      nix-output-monitor
      vim
    ] ++ lib.optional (system == "x86_64-linux") lnav; # https://github.com/tstack/lnav/issues/882
    variables = {
      EDITOR = "vim";
      PAGER = "less";
    };
    homeBinInPath = true;
    # zsh needs this for autoCompletion according to home-configuration
    # for fish, direnv suddenly stopped working, home-configuration suggested adding this and that seemed to have fixed it.
    # TODO occasionally see if I can safely remove this
    pathsToLink = [ "/share/zsh" "/share/fish" ];
    etc."words.txt".source =
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
  services.tuptime.enable = true;
  programs.bash.enableCompletion = true; # enable tab-completion for nix-* tools
  programs.mosh.enable = true;

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

  boot.cleanTmpDir = true;

  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
    '';
    # requireSignedBinaryCaches = false;
    # trustedUsers = [ "root" "@wheel" ];
    package = pkgs.nixUnstable;
    # https://github.com/gytis-ivaskevicius/flake-utils-plus/blob/master/modules/saneFlakeDefaults.nix
    registry =
      builtins.mapAttrs
        (name: v: { flake = v; })
        (lib.filterAttrs
          (name: value: value ? outputs)
          inputs);
  };
  programs.dconf.enable = true; # somehow necessary for home-manager to switch configurations?

}
