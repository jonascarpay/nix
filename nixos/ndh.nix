{ config, ... }:
{
  age.secrets.ndh-openvpn.file = ../secrets/ndh-openvpn-desktop.age;
  services.openvpn.servers.ndh-vpn = {
    autoStart = false;
    config = "config ${config.age.secrets.ndh-openvpn.path}";
  };
  networking.extraHosts = ''
    192.168.11.107	gitlab.ndh
    192.168.11.106	tx106.ndh
  '';
  home-manager.users.jmc.programs.ssh.matchBlocks = {
    "gitlab.ndh" = {
      user = "jcarpay";
      identityFile = "~/Keys/ssh/id_ndh";
    };
    "tx106.ndh" = {
      user = "jcarpay";
      identityFile = "~/Keys/ssh/id_ndh";
    };
  };
}
