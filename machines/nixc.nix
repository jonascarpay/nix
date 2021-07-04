{ config, lib, pkgs, ... }:
{
  imports = [
    (import ../system/zfs.nix "d2a9a7c0")
    ../system/fonts.nix
    ../system/global.nix
    ../system/jp.nix
    ../system/openvpn.nix
  ];

  networking = {
    hostName = "xc-jonas-desktop";
    networkmanager.enable = true;
    networkmanager.insertNameservers = [ "192.168.1.6" ];
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
        8081 # hoogle
        4040 # weird ssh
        4041 # weird http
        22000 # syncthing
      ];
      allowedUDPPorts = [
        53
        15515 # default
        139
        138 # samba
        27031
        27036 # steam
        8081 # steam
        21027 # syncthing
      ];
    };
  };

  hardware = {
    enableRedistributableFirmware = lib.mkDefault true;
    pulseaudio.enable = true;
    video.hidpi.enable = lib.mkDefault true;
    # bluetooth.enable = true;
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
  };

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" "sr_mod" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
    plymouth.enable = true;
    extraModulePackages = [ ];
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    blacklistedKernelModules = [ ];
    kernelParams = [
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
  };


  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/cc552c2e-f0bd-482c-a67e-efe476dfae98";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/5673-7655";
      fsType = "vfat";
    };
    # "/hdd" = {
    #   device = "/dev/disk/by-label/hdd";
    #   fsType = "ntfs";
    # };
  };
  environment = {
    systemPackages = with pkgs; [
      dnsutils
      linuxPackages.nvidia_x11
    ];
    variables.HM_PATH = "https://github.com/rycee/home-manager/archive/release-20.09.tar.gz";
  };

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  time.timeZone = "Asia/Tokyo";
  services = {
    printing = {
      enable = true;
      browsing = true;
      drivers = [ pkgs.brlaser pkgs.gutenprint pkgs.gutenprintBin ];
    };
    tlp = {
      enable = false;
      extraConfig = ''
        CPU_SCALING_GOVERNOR_ON_BAT=powersave
        CPU_SCALING_GOVERNOR_ON_AC=performance
      '';
    };
    udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="input", ATTR{name}=="TPPS/2 IBM TrackPoint", ATTR{device/sensitivity}="210"
    '';
    strongswan = {
      enable = true;
      secrets = [ "ipsec.d/ipsec.nm-l2tp.secrets" ];
    };

    fstrim.enable = true;
    xserver = {
      dpi = 140;
      libinput.enable = true;
      enable = true;
      videoDrivers = [ "nvidia" ];
      displayManager.autoLogin = {
        enable = true;
        user = "jmc";
      };
      desktopManager.plasma5.enable = true; # TODO disable
    };
    logind.extraConfig = "RuntimeDirectorySize=2G";
  };

  services.blueman.enable = true;

  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    requireSignedBinaryCaches = false;
  };

  programs.dconf.enable = true; # TODO ?
}
