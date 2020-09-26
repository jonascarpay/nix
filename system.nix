{ pkgs, config, ... }:

let
  unstable = import <unstable> { inherit (config.nixpkgs) config; };
in
{
  imports = [
    ./system-modules/openvpn.nix
    ./system-modules/mopidy.nix
    # ./system-modules/gnome.nix
    ./cachix.nix
    ./secrets.nix
  ];

  system.copySystemConfiguration = true;

  cachix = [ "all-hies" ];

  boot.kernelParams = [
    "noibrs"
    "noibpb"
    "nopti"
    "nospectre_v2"
    "nospectre_v1"
    "l1tf=off"
    "nospec_store_bypass_disable"
    "no_stf_barrier"
    "mds=off"
    "tsx=on"
    "tsx_async_abort=off"
    "mitigations=off"
  ];

  i18n = {
    defaultLocale = "ja_JP.UTF-8";
    inputMethod = {
      enabled = "fcitx";
      fcitx.engines = with pkgs.fcitx-engines; [ mozc table-other ];
    };
  };

  nixpkgs.config = {
    allowUnfree = true;
    # cudaSupport = true;
  };

  nix = {
    trustedUsers = [ "root" "jmc" ];
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    requireSignedBinaryCaches = false;
  };

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      nerdfonts
      corefonts
      powerline-fonts
      tewi-font
      # noto-fonts
      # noto-fonts-cjk
      # noto-fonts-emoji
      liberation_ttf
      # fira-code
      # fira-code-symbols
      # mplus-outline-fonts
      # dina-font
      # proggyfonts
      ## japanese
      kanji-stroke-order-font
      ipafont
      kochi-substitute
    ];
    fontconfig = {
      enable = true;
      allowBitmaps = false;
      defaultFonts = {
        monospace = [
          # "SauceCodePro Nerd Font Complete"
          "SauceCodePro Nerd Font"
          "IPAGothic"
        ];
        sansSerif = [ "DejaVu Sans" "IPAPGothic" ];
        serif = [ "DejaVu Serif" "IPAPMincho" ];
      };
    };
  };

  # time.timeZone = "Europe/Amsterdam";
  time.timeZone = "Asia/Tokyo";

  environment = {
    systemPackages = with pkgs; [
      file
      exfat
      git
      htop
      powertop
      libsForQt5.fcitx-qt5
      tree
    ];

    homeBinInPath = true;
    variables = {
      EDITOR = "vim";
      PAGER = "less";
      # HM_PATH = "https://github.com/rycee/home-manager/archive/master.tar.gz";
      HM_PATH = "https://github.com/rycee/home-manager/archive/release-20.03.tar.gz";
      # HM_PATH = "https://github.com/rycee/home-manager/archive/release-20.09.tar.gz";
      XMODIFIER = "@im=fcitx"; # Probably don't need this
    };
  };

  networking = {
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [
        22
        80 # ssh http
        139
        445
        27036
        27037 # steam
        8080 # hoogle
      ];
      allowedUDPPorts = [
        53
        15515 # default
        139
        138 # samba
        500 # xc vpn
        4500 # xc vpn
        27031
        27036 # steam
      ];
    };
  };

  security.sudo.wheelNeedsPassword = false;

  services = {
    openssh.enable = true;
    printing.enable = true;
    printing.drivers = [ pkgs.brlaser pkgs.gutenprint pkgs.gutenprintBin ];
    fstrim.enable = true;
    xserver = {
      enable = true;
      displayManager.lightdm.autoLogin = {
        enable = true;
        user = "jmc";
      };
      desktopManager.plasma5.enable = true;
    };

    strongswan = {
      enable = true;
      secrets = [ "ipsec.d/ipsec.nm-l2tp.secrets" ];
    };
    logind.extraConfig = "RuntimeDirectorySize=2G";
    ntp.enable = true;
    # nixosManual.showManual = true;
  };

  programs.bash.enableCompletion = true; # enable tab-completion for nix-* tools
  programs.fish.vendor.completions.enable =
    true; # not sure if this does anything for user fish but I'm willing to try

  programs.dconf.enable = true;
  users = {
    mutableUsers = false;
    users.jmc = {
      isNormalUser = true;
      description = "Jonas Carpay";
      createHome = true;
      extraGroups = [ "wheel" "networkmanager" "audio" ];
      openssh.authorizedKeys.keyFiles = [ /home/jmc/.ssh/id_rsa.pub ];
      # hashedPassword = "$6$l6ADy9OOpCno$fpzqJPb5oY4ESfo.4zVLA4F7EpIs1WaZSBsXlfCUx72IkeEIdI1/HjleKWFzc2L84mYX3Zhog3DfrBVMbLRgM0";
      hashedPassword = "$1$Nh9Zc.7Y$Zw9...mvYbA0qWE/PXKm7.";
    };
  };

}

# vim: fdm=indent:foldlevel=1:foldcolumn=2
