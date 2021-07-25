{ pkgs, unstable, lib, config, ... }:
let

  rclone = {
    networking.firewall.allowedTCPPorts = [ 8090 ];
    systemd.services = {
      rclone = {
        enable = true;
        script = ''
          ${pkgs.rclone}/bin/rclone serve webdav /tank/ --addr :8090 --user dav --pass dav
        '';
        wants = [ "network-online.target" ];
        after = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];
      };
    };
  };

  jellyfin = {
    services.jellyfin.enable = true;
    networking.firewall.allowedTCPPorts = [ 8096 8920 ];
    networking.firewall.allowedUDPPorts = [ 1900 7359 ];
  };

  transmission = {
    networking.firewall.allowedTCPPorts = [ 9091 ];
    services.openvpn.servers.nord-hk.autoStart = true;
    services.transmission = {
      enable = true;
      settings = {
        download-dir = "/tank/Transmission";
        incomplete-dir = "/tank/Transmission";
        rpc-whitelist = "127.0.0.1,192.168.1.*";
        rpc-bind-address = "0.0.0.0";
        utp-enabled = true;
      };
    };
  };

  syncthing = {
    # might no longer be necessary after kernel version > 5.10
    boot.kernel.sysctl."fs.inotify.max_user_watches" = 32784;
    networking.firewall.allowedTCPPorts = [ 8384 ]; # for GUI
    services.syncthing = {
      enable = true;
      package = unstable.syncthing;
      guiAddress = "0.0.0.0:8384";
    };
  };

  git-sync = {
    imports = [ ../system/git-sync-service.nix ];
    services.git-sync = {
      passwords.directory = "/tank/vault/Passwords";
      slipbox.directory = "/tank/vault/Slipbox";
      org.directory = "/tank/vault/Org";
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
      (import ../system/zfs.nix "3bf3504c")
      rclone
      jellyfin
      # transmission
      syncthing
      git-sync
    ];

  networking.hostName = "onigiri";
  networking.networkmanager.enable = true;
  time.timeZone = "Asia/Tokyo";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  system.stateVersion = "21.05";
  users.users.jmc.shell = pkgs.fish;
  hardware.raspberry-pi."4".fkms-3d.enable = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/3dd7b7cc-8570-4501-836f-fed55833d1c0";
    fsType = "ext4";
  };
}
