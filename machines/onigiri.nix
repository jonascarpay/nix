{ pkgs, lib, config, ... }:
{

  imports =
    [
      "${channels.nixos-hardware}/raspberry-pi/4/"
      ../secrets/accounts.nix
      ../system/global.nix
      ../system/openvpn.nix
      ../system/unbound.nix
      (import ../system/zfs.nix "3bf3504c")
    ];

  networking = {
    hostName = "onigiri";
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [
        8384 # syncthing GUI
        8096 # jellyfin HTTP
        8920 # jellyfin HTTPS
        22000 # syncthing
        8090 # rclone webdav # TODO move to module
        9091 # transmission
      ];
      allowedUDPPorts = [
        1900 # jellyfin
        7359 # jellyfin
        21027 # syncthing
      ];
    };
  };

  time.timeZone = "Asia/Tokyo";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  users.users.jmc = {
    shell = pkgs.fish;
  };

  services = {
    syncthing = {
      enable = true;
      package = channels.unstable.syncthing;
      dataDir = "/tank";
      guiAddress = "0.0.0.0:8384";
    };
    transmission = {
      enable = false;
      settings = {
        download-dir = "/tank/Transmission";
        incomplete-dir = "/tank/Transmission";
        rpc-whitelist = "192.168.1.*";
      };
    };

    jellyfin.enable = true;
  };

  services.journald.extraConfig = ''
    SystemMaxUse=16M
  '';

  system.stateVersion = "21.03";

  fileSystems = {
    "/" = {
      # device = "/dev/disk/by-uuid/44444444-4444-4444-8888-888888888888";
      device = "/dev/disk/by-uuid/3dd7b7cc-8570-4501-836f-fed55833d1c0";
      fsType = "ext4";
    };

    # "/boot" = {
    #   device = "/dev/disk/by-uuid/2178-694E";
    #   fsType = "vfat";
    # };

  };
  systemd.services = {
    rclone = {
      enable = true; # TODO re-enable
      script = ''
        ${pkgs.rclone}/bin/rclone serve webdav /tank/ --addr :8090 --user dav --pass dav
      '';
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
    };
  };

  hardware.raspberry-pi."4".fkms-3d.enable = true;
}
