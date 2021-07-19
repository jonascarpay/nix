{ pkgs, config, lib, ... }:
let
  serverdb = pkgs.fetchFromGitHub {
    owner = "jonascarpay";
    repo = "nord-openvpn-configs";
    rev = "5e9c413805174444a37ab2c0aaa45b2c3fe6962e";
    sha256 = "04rbkia4cp3yg4im3gh04kh6kjc12a18n4z0df3kjzffvyrh8fhl";
  };

  mkServ = serv: {
    config = ''
      ${builtins.readFile "${serverdb}/ovpn_tcp/${serv}.nordvpn.com.tcp.ovpn"}
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
