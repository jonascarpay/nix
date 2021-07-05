{ config, lib, pkgs, ... }:
{
  imports = [
    (import ../system/zfs.nix "d2a9a7c0")
    ../system/fonts.nix
    ../system/global.nix
    ../system/jp.nix
    ../system/openvpn.nix
    ../system/mlfa.nix
    ../system/graphical.nix
  ];

  networking = {
    hostName = "anpan";
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
    video.hidpi.enable = lib.mkDefault true;
  };

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" "sr_mod" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    blacklistedKernelModules = [ ];
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
  };
  environment = {
    systemPackages = with pkgs; [
      dnsutils
      linuxPackages.nvidia_x11
    ];
  };

  nix.maxJobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  time.timeZone = "Asia/Tokyo";
  services = {
    fstrim.enable = true;
    logind.extraConfig = "RuntimeDirectorySize=2G";
    xserver.videoDrivers = [ "nvidia" ];
    xserver.dpi = 140;
  };

  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    requireSignedBinaryCaches = false;
  };
  programs.dconf.enable = true; # TODO ?
}
