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

  adguard = {
    services.adguardhome = {
      enable = true;
      openFirewall = true;
      settings = {
        dns.upstream_dns = [
          "https://dns10.quad9.net/dns-query"
          "https://dns.mullvad.net/dns-query"
          "https://dns.nextdns.io"
        ];
        # During setup, the admin interface is indeed presented on the expected
        # port, but then after setup the dashboard is hosted on port 80. Maybe
        # this is intentional, but I find it confusing.
        http.address = "0.0.0.0:${config.services.adguardhome.port}"; # TODO what is this dumb workaround
        filtering.rewrites =
          let
            inherit (lib.attrsets) mapAttrsToList;
            servers = {
              "onigiri.lan" = "192.168.1.6";
              "mochi.lan" = "192.168.1.20";
              "norf.lan" = "192.168.1.208";
            };
          in
          mapAttrsToList (host: ip: { domain = host; answer = ip; }) servers
          ++ mapAttrsToList (host: ip: { domain = "*.${host}"; answer = ip; }) servers
        ;
      };
    };
    networking.firewall.allowedUDPPorts = [ 53 ];
  };

  tailscale = {
    services.tailscale = {
      enable = true;
      extraSetFlags = [ "--advertise-exit-node" ];
      # useRoutingFeatures = "both";
    };
    networking.firewall.checkReversePath = "loose"; # https://nixos.wiki/wiki/Tailscale#No_internet_when_using_an_exit_node
  };

in
{
  imports = [
    ../../nixos/global.nix
    ./otis.nix
    # ./domo.nix
    # zfs
    # rclone
    # syncthing
    # git-sync
    ./hardware-configuration.nix
    githubHosts
    # jellyfin
    paperless
    adguard
    tailscale
  ];

  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  security.rtkit.enable = true;

  nixpkgs.config.allowUnfree = true; # TODO global?

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
