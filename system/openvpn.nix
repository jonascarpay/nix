{ pkgs, config, lib, ... }:
let
  mkServ = serv: {
    config = ''
      ${builtins.readFile "${pkgs.flakes.nord-configs}/ovpn_tcp/${serv}.nordvpn.com.tcp.ovpn"}
      auth-user-pass ${config.age.secrets.openvpn.path}
    '';
    autoStart = lib.mkDefault false;
    updateResolvConf = true;
  };

in
{
  age.secrets.openvpn.file = ../secrets/openvpn.age;
  services.openvpn.servers = {
    nord-hk = mkServ "hk251";
    # nord-nl = mkServ "nl707" false;
    # nord-us = mkServ "us4629" false;
    nord-jp = mkServ "jp586";
    nord-ca = mkServ "ca977";
  };

}
