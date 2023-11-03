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

  git-sync = {
    imports = [ ./git-sync-service.nix ];
    services.git-sync = {
      passwords.directory = "/tank/Vault/Passwords";
      org.directory = "/tank/Vault/Org";
    };
  };

  zfs =
    let hostId = "3bf3504c";
    in {

      boot.supportedFilesystems = [ "zfs" ];
      networking.hostId = hostId;
      boot.zfs.extraPools = [ "tank" ];
      services.zfs.autoScrub.enable = true;
      services.zfs.autoSnapshot.enable = true;
    };

  fish = {
    users.users.jmc.shell = pkgs.fish;
    programs.fish.enable = true;
  };

in
{
  imports =
    [
      ../../nixos/global.nix
      ./unbound.nix
      ./wireguard.nix
      ./domo.nix
      zfs
      rclone
      # jellyfin
      # transmission
      syncthing
      git-sync
      fish
      inputs.nixos-hardware.nixosModules.raspberry-pi-4
    ];

  services.xserver.enable = false;
  hardware.raspberry-pi."4".fkms-3d.enable = true;

  boot.zfs.requestEncryptionCredentials = false;
  networking.hostName = "onigiri";
  networking.networkmanager.enable = true;
  time.timeZone = "Asia/Tokyo";
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

  users.groups = { tank.members = [ "jmc" ]; };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/3dd7b7cc-8570-4501-836f-fed55833d1c0";
    fsType = "ext4";
  };

  system.stateVersion = "21.05";
}
