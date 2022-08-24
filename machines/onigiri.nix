{ pkgs, lib, config, ... }:
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

  noip = {
    age.secrets.noip.file = ../secrets/noip.age;
    systemd.services.noip = {
      enable = true;
      description = "noip2 DNS service";
      wantedBy = [ "default.target" ];
      script = ''
        ${pkgs.noip}/bin/noip2 -c ${config.age.secrets.noip.path}
      '';
      serviceConfig = {
        Type = "forking";
        Restart = "always";
      };
    };
  };

  jellyfin = {
    services.jellyfin.enable = true;
    networking.firewall.allowedTCPPorts = [ 8096 8920 ];
    networking.firewall.allowedUDPPorts = [ 1900 7359 ];
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
    imports = [ ../system/git-sync-service.nix ];
    services.git-sync = {
      passwords.directory = "/tank/Vault/Passwords";
      slipbox.directory = "/tank/Vault/Slipbox";
      org.directory = "/tank/Vault/Org";
      programs-sqlite = rec {
        directory = "/home/jmc/programs.sqlite";
        preSync = ''
          cd ${directory}
          PATH=${lib.makeBinPath [ pkgs.curl pkgs.gnutar pkgs.xz pkgs.coreutils pkgs.bash pkgs.findutils]}:$PATH
          ./update.sh
        '';
        time = "daily";
        message = "programs.sqlite at $(${pkgs.coreutils}/bin/date -u)";
      };
      nord-openvpn-configs = {
        directory = "/home/jmc/nord-openvpn-configs";
        preSync = let rm = "${pkgs.coreutils}/bin/rm"; in
          ''
            set -eux
            ${rm} -rf ovpn_tcp
            ${rm} -rf ovpn_udp
            ${pkgs.curl}/bin/curl "https://downloads.nordcdn.com/configs/archives/servers/ovpn.zip" --output ovpn.zip
            ${pkgs.unzip}/bin/unzip -q ovpn.zip
            ${rm} ovpn.zip
          '';
        time = "daily";
        message = "Nord configs at $(${pkgs.coreutils}/bin/date -u)";
      };
    };
  };

in
{
  imports =
    [
      ../system/global.nix
      ../system/openvpn.nix
      ../system/unbound.nix
      ../system/wireguard.nix
      ../system/domo.nix
      (import ../system/zfs.nix "3bf3504c")
      rclone
      # jellyfin
      # transmission
      syncthing
      git-sync
      noip
    ];

  boot.zfs.requestEncryptionCredentials = false;
  networking.hostName = "onigiri";
  networking.networkmanager.enable = true;
  time.timeZone = "Asia/Tokyo";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  users.users.jmc.shell = pkgs.fish;
  hardware.raspberry-pi."4".fkms-3d.enable = true;

  users.groups = { tank.members = [ "jmc" ]; };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/3dd7b7cc-8570-4501-836f-fed55833d1c0";
    fsType = "ext4";
  };

  system.stateVersion = "21.05";
}
