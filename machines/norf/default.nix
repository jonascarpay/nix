{ pkgs, config, ... }:

let

  # wireless = {
  #   networking.networkmanager.enable = true;
  #   networking.networkmanager.insertNameservers = [ "192.168.1.6" ];
  # };

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

  transmission = {
    networking.firewall.allowedTCPPorts = [ config.services.transmission.settings.rpc-port ]; # Web UI port
    services.transmission = {
      enable = true;
      settings = {
        rpc-bind-address = "0.0.0.0";
        utp-enabled = true;
      };
      openFirewall = true;
      openRPCPort = true;
    };
    users.users.jmc.extraGroups = [ "transmission" ];
  };

  sound = {
    sound.enable = true;
    hardware.pulseaudio.enable = true;
    environment.systemPackages = [ pkgs.pavucontrol ];
  };

  udisks = {
    programs.gnome-disks.enable = true;
    services.udisks2.enable = true;
    services.udisks2.mountOnMedia = true;
    home-manager.users.jmc.services.udiskie.enable = true;
  };

  gnome-support = {
    # Applications like font-manager complain about missing services without this, see https://nixos.wiki/wiki/GNOME#Running_GNOME_programs_outside_of_GNOME
    programs.dconf.enable = true;
    environment.systemPackages = [ pkgs.gnome.adwaita-icon-theme ];
  };

  zfs = {
    boot.supportedFilesystems = [ "zfs" ];
    boot.zfs.forceImportRoot = false; # man configuration.nix recommends setting to false
    boot.zfs.passwordTimeout = 15;
    boot.zfs.extraPools = [ "tank" ];
    networking.hostId = "02e716d7";
    services.zfs.autoScrub.enable = true;
    services.zfs.autoSnapshot.enable = true;
    services.zfs.trim.enable = true;
  };

  boot-switch =
    let
      archive = pkgs.fetchzip {
        url = "https://github.com/CTCaer/hekate/releases/download/v6.0.2/hekate_ctcaer_6.0.2_Nyx_1.5.2.zip";
        sha256 = "sha256-I7WYxBIgBQjqwNlwIv6RRJ0LXTk5E3PsOC6c8/nJNxA=";
        stripRoot = false;
      };
      script = pkgs.writeShellScriptBin "boot-switch" ''
        ${pkgs.fusee-launcher}/bin/fusee-launcher ${archive}/hekate_ctcaer_6.0.2.bin
      '';
    in
    { environment.systemPackages = [ script ]; };


in

{
  imports =
    [
      ./hardware-configuration.nix
      sound
      graphics
      # wireless
      ./desktop.nix
      gnome-support
      transmission
      udisks
      zfs
      ../../nixos/global.nix
      ../../nixos/ndh.nix
    ];

  networking.useNetworkd = true;

  services.xserver.layout = "us"; # default from configuration.nix
  services.xserver.xkbVariant = ""; # default from configuration.nix
  boot.loader.systemd-boot.enable = true; # default from configuration.nix
  boot.loader.efi.canTouchEfiVariables = true; # default from configuration.nix

  networking.firewall.allowedTCPPorts = [ 8888 ];

  boot.loader.systemd-boot.memtest86.enable = true;

  networking.hostName = "norf";

  networking.resolvconf.dnsSingleRequest = true; # supposedly fixes slow DNS, see https://github.com/hashicorp/vagrant/issues/1172

  networking.interfaces.eno1.wakeOnLan.enable = true;

  time.timeZone = "Asia/Tokyo";
  i18n.defaultLocale = "en_US.UTF-8";

  environment.variables.ST_FONT = "SauceCodePro Nerd Font:pixelsize=24:antialias=true:autohint=true";

  services.mullvad-vpn.enable = true;
  services.mullvad-vpn.enableExcludeWrapper = true;

  system.stateVersion = "23.05";
}
