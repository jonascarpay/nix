{ pkgs, config, ... }:

let

  serverdb = pkgs.fetchzip {
    url = "https://downloads.nordcdn.com/configs/archives/servers/ovpn.zip";
    sha256 = "1mp447l9hak6akyaii0vdwbmn9nx0hz39iv0910b0hg35fvhppg7";
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
    nord-hk = mkServ "hk251" false;
    nord-nl = mkServ "nl707" false;
    # nord-us = mkServ "us4629" false;
    nord-ca = mkServ "ca977" false;
  };

}
