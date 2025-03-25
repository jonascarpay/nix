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
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
    };
    hardware.nvidia = {
      modesetting.enable = true;
      powerManagement.enable = true;
      open = true;
      nvidiaSettings = true;
    };
  };

  sound = {
    security.rtkit.enable = true; # TODO wiki recommends but why
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
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
    environment.systemPackages = [ pkgs.adwaita-icon-theme ]; # TODO dar-mode already includes this?
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

  docker = {
    virtualisation.docker = {
      enable = true;
      autoPrune.enable = true;
    };
    users.users.jmc.extraGroups = [ "docker" ];
  };

in

{
  imports =
    [
      ./hardware-configuration.nix
      sound
      graphics
      # wireless
      ./desktop-niri
      gnome-support
      udisks
      zfs
      ../../nixos/global.nix
      ../../nixos/ndh.nix
      docker
    ];

  # networking.useNetworkd = true;
  # networking.interfaces.eno1.useDHCP = true; # from hardware-config

  # default from configuration.nix
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  boot.loader.systemd-boot.enable = true; # default from configuration.nix
  boot.loader.efi.canTouchEfiVariables = true; # default from configuration.nix

  networking.firewall.allowedTCPPorts = [ 8888 8889 ];

  boot.loader.systemd-boot.memtest86.enable = true;

  environment.systemPackages = [ pkgs.linuxPackages.perf ];

  networking.hostName = "norf";
  # networking.resolvconf.dnsSingleRequest = true; # supposedly fixes slow DNS, see https://github.com/hashicorp/vagrant/issues/1172
  networking.networkmanager.enable = true;
  # networking.useDHCP = true;

  time.timeZone = "Asia/Tokyo";
  time.hardwareClockInLocalTime = true;
  i18n.defaultLocale = "en_US.UTF-8";

  # services.mullvad-vpn.enable = true;
  # services.mullvad-vpn.enableExcludeWrapper = true;

  # in man but broken?
  # services.ddcontrol.enable = true;

  system.stateVersion = "23.05";
}
