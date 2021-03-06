{ config, lib, pkgs, ... }:
let integrated = false;
in
{
  imports = [
    ../system/fonts.nix
    ../system/global.nix
    ../system/jp.nix
    ../system/openvpn.nix
  ];

  networking = {
    enableIPv6 = false;
    hostName = "xc-jonas";
    networkmanager = {
      enable = true;
    };
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
        500 # xc vpn
        4500 # xc vpn
        27031
        27036 # steam
        8081 # steam
        21027 # syncthing
      ];
    };
  };

  hardware = {
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      extraModules = [ pkgs.pulseaudio-modules-bt ]; # Necessary for bt headset
      # Prevents weird fade-ins in Anki -- hopefully
      extraConfig = if integrated then "" else ''
        unload-module module-suspend-on-idle
      '';
    };
    nvidiaOptimus.disable = integrated;
    opengl.driSupport32Bit = true;
    opengl.extraPackages = [ pkgs.linuxPackages.nvidia_x11.out ];
    opengl.extraPackages32 = [ pkgs.linuxPackages.nvidia_x11.lib32 ];
    nvidia.prime = {
      sync.enable = !integrated;
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
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
      device = "/dev/disk/by-uuid/f9c82637-f7c5-4384-9c37-1c8d9af3d1de";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/8024-F29A";
      fsType = "vfat";
    };
  };

  environment = {
    systemPackages = with pkgs; [
      acpi
      powertop
      linuxPackages.nvidia_x11
    ];
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
      videoDrivers = [
        (if integrated then "intel" else "nvidia")
      ];
      displayManager.autoLogin = {
        enable = true;
        user = "jmc";
      };
      desktopManager.plasma5.enable = true; # TODO disable
    };
    logind.extraConfig = "RuntimeDirectorySize=2G";
  };

  hardware.bluetooth.enable = true;
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
