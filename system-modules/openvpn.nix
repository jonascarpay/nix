{ pkgs, config, ... }:

let

  serverdb = pkgs.fetchzip {
    url = "https://downloads.nordcdn.com/configs/archives/servers/ovpn.zip";
    sha256 = "03ypwwc7qmr42f06z64agyzjqd2591bkwm3562qpr38f79acyldd";
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
    nord-hk = mkServ "hk137" false;
    # nord-jp = mkServ "jp247" false; # No longer exists
    nord-nl = mkServ "nl707" false;
    nord-us = mkServ "us4629" false;
    nord-ca = mkServ "ca977" false;
  };

}
