{ lib, pkgs, ... }:

let

  wireless = {
    networking.networkmanager.enable = true;
    networking.networkmanager.insertNameservers = [ "192.168.1.6" ];
  };

  syncthing = {
    home-manager.users.jmc.services.syncthing = {
      enable = true;
      tray.enable = true;
      tray.command = "syncthingtray --wait";
    };
    # https://docs.syncthing.net/users/firewall.html
    networking.firewall.allowedTCPPorts = [ 22000 ];
    networking.firewall.allowedUDPPorts = [ 22000 21027 ];
  };

  graphics = {
    services.xserver = {
      enable = true;
      videoDrivers = [ "nvidia" ];
    };
    hardware.opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
    hardware.nvidia = {
      modesetting.enable = true;
      powerManagement.enable = true;
      open = true;
      nvidiaSettings = true;
    };
  };

  sound = {
    sound.enable = true;
    hardware.pulseaudio.enable = true;
    environment.systemPackages = [ pkgs.pavucontrol ];
  };

  desktop = {
    imports = [
      ../../nixos/home-manager-xsession.nix
      polybar
    ];
    home-manager.users.jmc = import ../../desktop;
  };

  gnome-support = {
    # Applications like font-manager complain about missing services without this, see https://nixos.wiki/wiki/GNOME#Running_GNOME_programs_outside_of_GNOME
    programs.dconf.enable = true;
    environment.systemPackages = [ pkgs.gnome.adwaita-icon-theme ];
  };

  polybar = {
    home-manager.users.jmc = {
      imports = [ ../../desktop/polybar.nix ];
      services.polybar.settings = {
        "bar/mybar" = {
          "inherit" = "bar/common bar/hidpi";
          modules-right = "notifications onigiri wireless vpn fs memory cpu-temp gpu-temp cpu pulseaudio date-nl date";
        };
        "module/wireless".interface = "wlp9s0";
        "module/cpu-temp".hwmon-path = "/sys/devices/pci0000:00/0000:00:18.3/hwmon/hwmon2/temp1_input";
        "module/gpu-temp".hwmon-path = "/sys/devices/pci0000:00/0000:00:08.1/0000:0d:00.0/hwmon/hwmon5/temp1_input";
      };
    };
  };


in

{
  imports =
    [
      sound
      graphics
      desktop
      syncthing
      wireless
      ./hardware-configuration.nix
      gnome-support
      ../../nixos/global.nix
      ../../nixos/fonts.nix
      ../../nixos/ndh.nix
    ];

  services.xserver.layout = "us"; # default from configuration.nix
  services.xserver.xkbVariant = ""; # default from configuration.nix
  boot.loader.systemd-boot.enable = true; # default from configuration.nix
  boot.loader.efi.canTouchEfiVariables = true; # default from configuration.nix

  networking.hostName = "norf";

  networking.resolvconf.dnsSingleRequest = true; # supposedly fixes slow DNS, see https://github.com/hashicorp/vagrant/issues/1172

  time.timeZone = "Asia/Tokyo";
  i18n.defaultLocale = "en_US.UTF-8";

  environment.variables.ST_FONT = "DM Mono Nerd Font:pixelsize=24:antialias=true:autohint=true";

  services.mullvad-vpn.enable = true;
  services.mullvad-vpn.enableExcludeWrapper = true;

  system.stateVersion = "23.05";
}
