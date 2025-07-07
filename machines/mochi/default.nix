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
    services.jellyfin = {
      enable = true;
      openFirewall = true;
    };
    systemd.services.jellyfin = {
      bindsTo = [ "tank-Vault.mount" ];
      after = [ "tank-Vault.mount" ];
    };
  };

  paperless = {
    services.paperless = {
      enable = true;
      address = "0.0.0.0";
      settings.PAPERLESS_OCR_LANGUAGE = "eng+jpn+nld";
      exporter.enable = true;
      exporter.directory = "/tank/PaperlessExports/";
    };
    networking.firewall.allowedTCPPorts = [ config.services.paperless.port ];
    systemd.services.paperless-exporter = {
      after = [ "tank.mount" ];
      bindsTo = [ "tank.mount" ];
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
        /tank 192.168.1.208(rw,sync,insecure,all_squash,anongid=100,anonuid=1000)
        /tank 100.117.239.92(rw,sync,insecure,all_squash,anongid=100,anonuid=1000)
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
  ];

  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

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
