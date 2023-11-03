hostId: {
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = hostId;
  boot.zfs.extraPools = [ "tank" ];
  services.zfs.autoScrub.enable = true;
  services.zfs.autoSnapshot.enable = true;
}
