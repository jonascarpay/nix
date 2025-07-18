{ pkgs, ... }:
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
    extraGroups = [ "docker" "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKQ90WJQvcslYhHgaSZmqldOSE0uMMVTFdcvWJU0GK72 ocarpaca115@alumnes.ub.edu"
    ];
    shell = pkgs.fish;
  };
  networking.firewall.allowedTCPPorts = [ 20007 ];
}
