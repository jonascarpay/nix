{ config, lib, pkgs, ... }:
let

  transmission = {
    # networking.firewall.allowedTCPPorts = [ 9091 ];
    systemd.services."transmission" = {
      bindsTo = [ "openvpn-nord.service" ];
      wantedBy = lib.mkForce [ ];
    };
    services.transmission = {
      enable = true;
      settings = {
        download-dir = "/tank/Transmission";
        incomplete-dir = "/tank/Transmission";
        # rpc-whitelist = "127.0.0.1,192.168.1.*";
        rpc-bind-address = "0.0.0.0";
        utp-enabled = true;
      };
    };
  };

in
{
  imports = [
    (import ../system/zfs.nix "d2a9a7c0")
    ../system/fonts.nix
    ../system/global.nix
    ../system/jp.nix
    ../system/openvpn.nix
    ../system/mlfa.nix
    ../system/graphical.nix
    transmission
  ];

  networking = {
    hostName = "anpan";
    networkmanager.enable = true;
    networkmanager.insertNameservers = [ "192.168.1.6" ];
  };

  hardware.video.hidpi.enable = true;

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" "sr_mod" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
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

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  time.timeZone = "Asia/Tokyo";
  services = {
    logind.extraConfig = "RuntimeDirectorySize=2G";
    xserver.dpi = 140;
    xserver.videoDrivers = lib.mkDefault [ "nvidia" ];
    ## Wii U USB adapter rule
    udev.extraRules = ''
      SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTRS{idVendor}=="057e", ATTRS{idProduct}=="0337", MODE="0666"
    '';
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
