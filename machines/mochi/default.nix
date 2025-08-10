{ pkgs, lib, config, inputs, ... }:
let

  # TODO: filebrowser, currently in 25.11
  # TODO: vaultwarden

  jellyfin = {
    services.jellyfin = {
      enable = true;
      openFirewall = true;
    };
    systemd.services.jellyfin = {
      bindsTo = [ "tank.mount" ];
      after = [ "tank.mount" ];
    };
    services.caddy.virtualHosts."jellyfin.mochi.lan".extraConfig = ''
      tls internal
      reverse_proxy localhost:8096
    '';
  };

  immich = {
    services.immich = {
      enable = true;
      host = "0.0.0.0";
      openFirewall = true;
      mediaLocation = "/tank/Immich";
    };
    systemd.services.immich = {
      after = [ "tank.mount" ];
      bindsTo = [ "tank.mount" ];
    };
    services.caddy.virtualHosts."immich.mochi.lan".extraConfig = ''
      tls internal
      reverse_proxy localhost:${builtins.toString(config.services.immich.port)}
    '';
  };

  paperless = {
    services.paperless = {
      enable = true;
      address = "0.0.0.0"; # MARK
      settings.PAPERLESS_OCR_LANGUAGE = "eng+jpn+nld";
      settings.PAPERLESS_URL = "https://paperless.mochi.lan"; # MARK
      exporter.enable = true;
      exporter.directory = "/tank/PaperlessExports/";
    };
    networking.firewall.allowedTCPPorts = [ config.services.paperless.port ]; # MARK
    systemd.services.paperless-exporter = {
      after = [ "tank.mount" ];
      bindsTo = [ "tank.mount" ];
    };
    services.caddy.virtualHosts."paperless.mochi.lan".extraConfig = ''
      tls internal
      reverse_proxy localhost:${builtins.toString(config.services.paperless.port)}
    '';
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
    networking.hostId = "8c2565a1";
    # boot.zfs.extraPools = [ "tank" ];
    boot.zfs.forceImportRoot = false; # man configuration.nix recommends setting to false
    boot.zfs.requestEncryptionCredentials = false;
    services.zfs.autoScrub.enable = true;
    services.zfs.autoSnapshot.enable = true;
  };

  nfs = {
    # https://nixos.wiki/wiki/ZFS#NFS_shares
    services.nfs.server = {
      enable = true;
      lockdPort = 4001;
      mountdPort = 4002;
      statdPort = 4000;
      exports = ''
        /tank          192.168.1.0/24(rw,sync,insecure,all_squash,anongid=100,anonuid=1000)
        /tank/NAS/jmc  100.117.239.92(rw,sync,insecure,all_squash,anongid=100,anonuid=1000) # jmbp
        /tank/NAS/otis 100.127.99.65(rw,sync,insecure,all_squash,anongid=100,anonuid=1001)  # kimberley
      '';
    };
    networking.firewall =
      let
        ports = [ 111 2049 4000 4001 4002 20048 ];
      in
      {
        allowedTCPPorts = ports;
        allowedUDPPorts = ports;
      };
    systemd.services.nfs-server = {
      after = [ "tank.mount" ];
      bindsTo = [ "tank.mount" ];
    };
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
            mochi-ts = "100.91.139.59";
          in
          [
            { domain = "mochi.lan"; answer = mochi-ts; }
            { domain = "*.mochi.lan"; answer = mochi-ts; }
          ];
      };
    };
    networking.firewall.allowedUDPPorts = [ 53 ];
    services.caddy.virtualHosts."adguard.mochi.lan".extraConfig = ''
      tls internal
      reverse_proxy localhost:${builtins.toString(config.services.adguardhome.port)}
    '';
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
    ./hass.nix
    # ./domo.nix
    zfs
    nfs
    # rclone
    # syncthing
    # git-sync
    ./hardware-configuration.nix
    githubHosts
    jellyfin
    paperless
    adguard
    tailscale
    immich
  ];

  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  services.caddy.enable = true;

  security.rtkit.enable = true;

  nixpkgs.config.allowUnfree = true; # TODO global?

  networking.hostName = "mochi";
  time.timeZone = "Asia/Tokyo";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  services.getty.autologinUser = "jmc";

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  i18n.defaultLocale = "en_US.UTF-8";

  system.stateVersion = "23.11";
}
