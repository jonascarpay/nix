{ pkgs, lib, config, inputs, ... }:
let

  rclone = {
    networking.firewall.allowedTCPPorts = [ 8090 ];
    systemd.services = {
      rclone = {
        enable = true;
        script = ''
          ${pkgs.rclone}/bin/rclone serve webdav /tank/Vault/ --addr :8090 --user dav --pass dav
        '';
        bindsTo = [ "tank-Vault.mount" ];
        after = [ "tank-Vault.mount" ];
      };
    };
  };

  syncthing = {
    # might no longer be necessary after kernel version > 5.10
    boot.kernel.sysctl."fs.inotify.max_user_watches" = 32784;
    networking.firewall.allowedTCPPorts = [ 8384 ]; # for GUI
    systemd.services.syncthing = {
      bindsTo = [ "tank-Vault.mount" ];
      after = [ "tank-Vault.mount" ];
    };
    services.syncthing = {
      enable = true;
      user = "jmc";
      group = "tank";
      guiAddress = "0.0.0.0:8384";
    };
  };

  git-sync = {
    imports = [ ./git-sync-service.nix ];
    services.git-sync = {
      passwords.directory = "/tank/Vault/Passwords";
      org.directory = "/tank/Vault/Org";
    };
  };

  zfs = {
    boot.supportedFilesystems = [ "zfs" ];
    networking.hostId = "3bf3504c";
    boot.zfs.extraPools = [ "tank" ];
    services.zfs.autoScrub.enable = true;
    services.zfs.autoSnapshot.enable = true;
  };

  fish = {
    users.users.jmc.shell = pkgs.fish;
    programs.fish.enable = true;
  };

in
{
  imports =
    [
      ../../nixos/global.nix
      # ./unbound.nix
      # ./wireguard.nix
      # ./domo.nix
      # zfs
      # rclone
      # jellyfin
      # transmission
      # syncthing
      # git-sync
      fish
      ./hardware-configuration.nix
      # inputs.nixos-hardware.nixosModules.raspberry-pi-4
    ];

  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  security.rtkit.enable = true;

  # systemd.services."getty@tty1".enable = false;
  # systemd.services."autovt@tty1".enable = false;

  nixpkgs.config.allowUnfree = true;


  # boot.zfs.requestEncryptionCredentials = false;
  networking.hostName = "mochi";
  networking.networkmanager.enable = true;
  time.timeZone = "Asia/Tokyo";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  services.getty.autologinUser = "jmc";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "ja_JP.UTF-8";
    LC_IDENTIFICATION = "ja_JP.UTF-8";
    LC_MEASUREMENT = "ja_JP.UTF-8";
    LC_MONETARY = "ja_JP.UTF-8";
    LC_NAME = "ja_JP.UTF-8";
    LC_NUMERIC = "ja_JP.UTF-8";
    LC_PAPER = "ja_JP.UTF-8";
    LC_TELEPHONE = "ja_JP.UTF-8";
    LC_TIME = "ja_JP.UTF-8";
  };


  system.stateVersion = "23.11";
}
