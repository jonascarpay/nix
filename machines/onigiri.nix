{ pkgs, unstable, lib, config, ... }:
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
      package = unstable.syncthing;
      guiAddress = "0.0.0.0:8384";
    };
  };

  git-sync = {
    imports = [ ../system/git-sync-service.nix ];
    services.git-sync = {
      passwords.directory = "/tank/Vault/Passwords";
      slipbox.directory = "/tank/Vault/Slipbox";
      org.directory = "/tank/Vault/Org";
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
      (import ../system/zfs.nix "3bf3504c")
      rclone
      jellyfin
      # transmission
      syncthing
      git-sync
    ];

  boot.zfs.requestEncryptionCredentials = false;
  networking.hostName = "onigiri";
  networking.networkmanager.enable = true;
  time.timeZone = "Asia/Tokyo";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  system.stateVersion = "21.05";
  users.users.jmc.shell = pkgs.fish;
  hardware.raspberry-pi."4".fkms-3d.enable = true;

  users.groups = { tank.members = [ "jmc" ]; };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/3dd7b7cc-8570-4501-836f-fed55833d1c0";
    fsType = "ext4";
  };
}
