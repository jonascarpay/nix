{ pkgs, config, ... }:
let
  serverdb = pkgs.fetchzip {
    url = "https://downloads.nordcdn.com/configs/archives/servers/ovpn.zip";
    sha256 = "0dzc6ays0nx7sjkw5r1zyqv62b929izznck58vsj35cr1cy03ni5";
    stripRoot = false;
  };

  mkServ = serv: auto: {
    config =
      builtins.readFile "${serverdb}/ovpn_tcp/${serv}.nordvpn.com.tcp.ovpn";
    autoStart = auto;
    updateResolvConf = true;
    authUserPass = config.secrets.openvpn;
  };

in
{

  services.openvpn.servers = {
    nord-hk = mkServ "hk251" false;
    # nord-nl = mkServ "nl707" false;
    # nord-us = mkServ "us4629" false;
    nord-ca = mkServ "ca977" false;
  };

}
