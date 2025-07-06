{
  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
    rootless.enable = true;
  };
  users.users.jmc.extraGroups = [ "docker" ];
  users.users.otis = {
    isNormalUser = true;
    description = "Otis Carpay";
    createHome = true;
    extraGroups = [ "docker" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKQ90WJQvcslYhHgaSZmqldOSE0uMMVTFdcvWJU0GK72 ocarpaca115@alumnes.ub.edu"
    ];
  };
  networking.firewall.allowedTCPPorts = [ 20007 ];
}
