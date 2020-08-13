{ pkgs, config, ... }:

let

  serverdb = pkgs.fetchzip {
    url = "https://downloads.nordcdn.com/configs/archives/servers/ovpn.zip";
    sha256 = "0k1ym0z89j78pq70qq09vaw35izqxnmwnnfyac364ws2bj2yiqzn";
    stripRoot = false;
  };

  mkServ = serv: auto: {
    config =
      builtins.readFile "${serverdb}/ovpn_tcp/${serv}.nordvpn.com.tcp.ovpn";
    autoStart = auto;
    updateResolvConf = true;
    authUserPass = config.secrets.openvpn;
  };

in {

  services.openvpn.servers = {
    nord-hk = mkServ "hk143" false;
    nord-nl = mkServ "nl707" false;
    nord-us = mkServ "us4629" false;
    nord-ca = mkServ "ca977" false;
  };

}
