{ pkgs, config, ... }:
let
  serverdb = pkgs.fetchzip {
    url = "https://downloads.nordcdn.com/configs/archives/servers/ovpn.zip";
    sha256 = "sha256-ZCiU/bTzL1mm5+f/k+/GrJne+M3L5ZRrcuit7gYFfCw=";
    stripRoot = false;
  };

  mkServ = serv: auto: {
    config = ''
      ${builtins.readFile "${serverdb}/ovpn_tcp/${serv}.nordvpn.com.tcp.ovpn"}
      auth-user-pass ${config.age.secrets.openvpn.path}
    '';
    autoStart = auto;
    updateResolvConf = true;
  };

in
{
  age.secrets.openvpn.file = ../secrets/openvpn.age;
  services.openvpn.servers = {
    nord-hk = mkServ "hk251" false;
    # nord-nl = mkServ "nl707" false;
    # nord-us = mkServ "us4629" false;
    nord-jp = mkServ "jp586" false;
    nord-ca = mkServ "ca977" false;
  };

}
