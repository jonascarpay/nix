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

  noip = {
    age.secrets.noip.file = ../../secrets/noip.age;
    systemd.services.noip = {
      description = "No-IP Dynamic DNS Update Client";
      after = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
      requires = [ "network-online.target" ];
      serviceConfig = {
        Type = "forking";
        ExecStart = "${pkgs.noip}/bin/noip2 -c ${config.age.secrets.noip.path}";
        restart = "on-failure";
        restartSec = 5;
      };
    };
  };

  jellyfin = {
    users.users.jmc.homeMode = "701"; # Media is currently stored in my home dir
    services.jellyfin = {
      enable = true;
      openFirewall = true;
    };
  };

  paperless = {
    services.paperless = {
      enable = true;
      address = "0.0.0.0";
      consumptionDirIsPublic = true;
      extraConfig.PAPERLESS_OCR_LANGUAGE = "eng+jpn+nld";
    };
    networking.firewall.allowedTCPPorts = [ config.services.paperless.port ];
  };

  makasete = {
    systemd = {
      services.makasete = {
        description = "Makasete";
        script = "${inputs.makasete.packages.${pkgs.system}.default}/bin/makasete";
        wantedBy = [ "default.target" ];
      };
      targets.makasete.after = [ "network.target" ];
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

  githubHosts = {
    home-manager.users.jmc.programs.ssh.matchBlocks."github.com" = {
      user = "git";
      identityFile = "/home/jmc/Keys/ssh/id_ed25519";
    };
  };

in
{
  imports = [
    ../../nixos/global.nix
    # ./unbound.nix
    ./wireguard.nix
    ./otis.nix
    # ./domo.nix
    # zfs
    # rclone
    # jellyfin
    # transmission
    # syncthing
    # git-sync
    ./hardware-configuration.nix
    ../onigiri/unbound.nix
    githubHosts
    jellyfin
    paperless
    makasete
    noip
  ];

  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  security.rtkit.enable = true;

  # systemd.services."getty@tty1".enable = false;
  # systemd.services."autovt@tty1".enable = false;

  nixpkgs.config.allowUnfree = true;

  # boot.zfs.requestEncryptionCredentials = false;
  networking.hostName = "mochi";
  time.timeZone = "Asia/Tokyo";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  services.getty.autologinUser = "jmc";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  i18n.defaultLocale = "en_US.UTF-8";

  system.stateVersion = "23.11";
}
