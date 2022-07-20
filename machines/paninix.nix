{ config, lib, pkgs, ... }:
let
  integrated = true;
  wireguard = let port = 51820; in
    {
      age.secrets.wg-paninix.file = ../secrets/wg-paninix.age;
      networking.wg-quick.interfaces.wg0 = {
        address = [ "10.100.0.3/24" ];
        dns = [ "192.168.1.1" ];
        mtu = 1384;
        privateKeyFile = config.age.secrets.wg-paninix.path;
        peers = [{
          publicKey = "1DQu3fqcOew1Mxvpiq/0umajstSXEzdcfhY89dcHkHw=";
          allowedIPs = [ "192.168.1.0/24" ]; # Only route internal traffic through wg
          endpoint = "126.51.120.49:${builtins.toString port}";
          persistentKeepalive = 25;
        }];
      };
    };

  bluetooth = {
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;
  };
in
{
  # TODO use graphical.nix
  imports = [
    ../system/fonts.nix
    ../system/global.nix
    ../system/jp.nix
    ../system/openvpn.nix
    ../system/graphical.nix
    wireguard
    bluetooth
  ];

  networking = {
    enableIPv6 = false;
    hostName = "xc-jonas";
    networkmanager = {
      enable = true;
      insertNameservers = [ "1.1.1.1" ];
    };
  };

  hardware = {
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
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
    trackpoint.sensitivity = 210;
  };

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
    kernelModules = [ "kvm-intel" ]; # virtualization
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    kernelParams = [ "mitigations=off" ];
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

  time.timeZone = "Europe/Amsterdam";
  services = {
    printing = {
      enable = true;
      browsing = true;
      drivers = [ pkgs.brlaser pkgs.gutenprint pkgs.gutenprintBin ];
    };
    udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/intel_backlight/brightness"
    '';
    # This rule should work for brightness, but doesn't for some reason:
    # ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", GROUP="video", MODE="0666"

    xserver = {
      dpi = 140;
      libinput.enable = true;
      videoDrivers = [ (if integrated then "intel" else "nvidia") ];
    };
    logind.extraConfig = "RuntimeDirectorySize=2G";
  };

  system.stateVersion = "21.05";
}
