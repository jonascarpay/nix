{ pkgs, lib, config, ... }:
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

  transmission =
    let
      enable = false;
    in
    {
      networking.firewall.allowedTCPPorts = [ 9091 ];
      services.openvpn.servers.nord-hk.autoStart = enable;
      services.transmission = {
        inherit enable;
        settings = {
          download-dir = "/tank/Transmission";
          incomplete-dir = "/tank/Transmission";
          rpc-whitelist = "192.168.1.*";
        };
      };
    };

  syncthing = {
    networking.firewall.allowedTCPPorts = [ 8384 ]; # for GUI
    services.syncthing = {
      enable = true;
      package = pkgs.unstable.syncthing;
      dataDir = "/tank";
      guiAddress = "0.0.0.0:8384";
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
      transmission
      syncthing
    ];

  networking.hostName = "onigiri";
  networking.networkmanager.enable = true;
  time.timeZone = "Asia/Tokyo";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
  system.stateVersion = "21.05";
  users.users.jmc.shell = pkgs.fish;
  hardware.raspberry-pi."4".fkms-3d.enable = true;

  services.journald.extraConfig = ''
    SystemMaxUse=16M
  '';

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/3dd7b7cc-8570-4501-836f-fed55833d1c0";
    fsType = "ext4";
  };
}
