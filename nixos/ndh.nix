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
  home-manager.users.jmc =
    let
      config = {
        user = "jcarpay";
        identityFile = "~/Keys/ssh/id_ndh";
      };
    in
    { lib, ... }: {
      programs.ssh.matchBlocks = {
        "gitlab.ndh" = config;
        "tx101.ndh" = config;
        "tx102.ndh" = config;
        "tx105.ndh" = config;
        "tx106.ndh" = config;
        "tx108.ndh" = config;
        "jp104.ndh" = lib.hm.dag.entryAfter [ "tx108.ndh" ] {
          inherit (config) user identityFile;
          hostname = "192.168.183.104";
          proxyJump = "tx108.ndh";
        };
        "jp109.ndh" = lib.hm.dag.entryAfter [ "tx108.ndh" ] {
          inherit (config) user identityFile;
          hostname = "192.168.183.109";
          proxyJump = "tx108.ndh";
        };
        "jp108.ndh" = lib.hm.dag.entryAfter [ "tx108.ndh" ] {
          inherit (config) user identityFile;
          hostname = "192.168.183.108";
          proxyJump = "tx108.ndh";
        };
        "jp107.ndh" = lib.hm.dag.entryAfter [ "tx108.ndh" ] {
          inherit (config) user identityFile;
          hostname = "192.168.183.107";
          proxyJump = "tx108.ndh";
        };
        "jp110.ndh" = lib.hm.dag.entryAfter [ "tx108.ndh" ] {
          inherit (config) user identityFile;
          hostname = "192.168.183.110";
          proxyJump = "tx108.ndh";
        };
        "jp113.ndh" = lib.hm.dag.entryAfter [ "tx108.ndh" ] {
          inherit (config) user identityFile;
          hostname = "192.168.183.113";
          proxyJump = "tx108.ndh";
        };
      };
    };
}
