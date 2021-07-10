{ pkgs, config, ... }:
let
  serverdb = pkgs.fetchFromGitHub {
    owner = "jonascarpay";
    repo = "nord-openvpn-configs";
    rev = "b7c3e7f90a63423fbe3f57d8b6f2e03a18c8dc07";
    sha256 = "sha256-8UOScq9+FDOpwst9HmlOTiZDpf/lz64UIWonvMcbkp8=";
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
