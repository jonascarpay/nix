{ config, ... }:
{
  age.secrets.ndh-openvpn.file = ../secrets/ndh-openvpn-desktop.age;
  services.openvpn.servers.ndh-vpn = {
    autoStart = false;
    config = "config ${config.age.secrets.ndh-openvpn.path}";
  };
  networking.extraHosts = ''
    192.168.11.101	tx101.ndh
    192.168.11.102	tx102.ndh
    192.168.11.105	tx105.ndh
    192.168.11.106	tx106.ndh
    192.168.11.107	gitlab.ndh
    192.168.11.108	tx108.ndh
  '';
  home-manager.users.jmc.programs.ssh.matchBlocks =
    let
      config = {
        user = "jcarpay";
        identityFile = "~/Keys/ssh/id_ndh";
      };
    in
    {
      "gitlab.ndh" = config;
      "tx101.ndh" = config;
      "tx102.ndh" = config;
      "tx105.ndh" = config;
      "tx106.ndh" = config;
      "tx108.ndh" = config;
    };
}
