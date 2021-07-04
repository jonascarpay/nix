{ pkgs, lib, config, ... }:
let
  channels = import ../channels.nix;
in
{

  imports =
    [
      "${channels.nixos-hardware}/raspberry-pi/4/"
      ../secrets/accounts.nix
      ../system/global.nix
      ../system/openvpn.nix
      ../system/unbound.nix
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
      dataDir = "/mnt/exthd";
      guiAddress = "0.0.0.0:8384";
    };
    transmission = {
      enable = false;
      openFirewall = true;
      settings = {
        download-dir = "/mnt/exthd/Transmission";
        incomplete-dir = "/mnt/exthd/Transmission";
        rpc-whitelist = "192.168.1.3";
      };
    };

    jellyfin.enable = true;
    # jellyfin.package = channels.unstable.jellyfin;
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

    "/mnt/exthd" = {
      device = "/dev/disk/by-uuid/2A76DE2B76DDF811";
      fsType = "ntfs";
      neededForBoot = false;
      noCheck = true;
    };
  };
  systemd.services = {
    rclone = {
      enable = false; # TODO re-enable
      script = ''
        ${pkgs.rclone}/bin/rclone serve webdav /mnt/exthd/ --addr :8090 --user dav --pass dav
      '';
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
    };
  };

  hardware.raspberry-pi."4".fkms-3d.enable = true;
}

# vim: fdm=indent:foldlevel=2:foldcolumn=2
