{ config, lib, pkgs, ... }:
let

  transmission = {
    # networking.firewall.allowedTCPPorts = [ 9091 ];
    systemd.services."transmission" = {
      wantedBy = lib.mkForce [ ]; # TODO I don't remember why this is here
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
    users.users.jmc.extraGroups = [ "transmission" ];
  };

  docker = {
    virtualisation.docker = {
      enable = true;
      autoPrune = {
        enable = true;
        flags = [ "--all" ];
      };
    };
    virtualisation.virtualbox.host.enable = true;
    virtualisation.virtualbox.host.enableExtensionPack = true;
    users.users.jmc.extraGroups = [ "docker" "vboxusers" ];
  };

in
{
  # TODO move these to flake.nix, especially things like "graphical"
  imports = [
    (import ../system/zfs.nix "d2a9a7c0")
    ../system/fonts.nix
    ../system/global.nix
    ../system/jp.nix
    ../system/openvpn.nix
    ../system/graphical.nix
    transmission
    docker
  ];

  networking = {
    hostName = "anpan";
    networkmanager.enable = true;
    networkmanager.insertNameservers = [ "192.168.1.6" ];
  };

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" "sr_mod" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    # loader.systemd-boot.enable = true;
    # loader.efi.canTouchEfiVariables = true;
    blacklistedKernelModules = [ ];
    loader.grub.enable = true;
    loader.grub.device = "/dev/nvme0n1";
    loader.grub.useOSProber = true;
    # kernelParams = [ "mitigations=off" ];
  };

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/82a81e01-e401-45b6-b5c6-e901cd225f18";
      fsType = "ext4";
    };

  # fileSystems = {
  #   "/" = {
  #     device = "/dev/disk/by-uuid/cc552c2e-f0bd-482c-a67e-efe476dfae98";
  #     fsType = "ext4";
  #   };
  #   "/boot" = {
  #     device = "/dev/disk/by-uuid/5673-7655";
  #     fsType = "vfat";
  #   };
  # };

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

  system.stateVersion = "21.05";
}
