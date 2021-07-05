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
