{ config, pkgs, ... }:
let
  port = 51820;
  ext = "eth0";
  iptables = "${pkgs.iptables}/bin/iptables";
in
{
  age.secrets.wg-onigiri.file = ../secrets/wg-onigiri.age;
  networking = {
    nat.enable = true;
    nat.externalInterface = ext;
    nat.internalInterfaces = [ "wg0" ];
    firewall.allowedUDPPorts = [ port ];
  };
  networking.wireguard.interfaces.wg0 = {
    ips = [ "10.100.0.1/24" ];
    listenPort = port;
    postSetup = ''
      ${iptables} -t nat -A POSTROUTING -s 10.100.0.0/24 -o ${ext} -j MASQUERADE
    '';
    postShutdown = ''
      ${iptables} -t nat -D POSTROUTING -s 10.100.0.0/24 -o ${ext} -j MASQUERADE
    '';
    privateKeyFile = config.age.secrets.wg-onigiri.path;
    peers = [
      {
        # iPhone
        publicKey = "yHZaq+bpKBOz1qDXKwtnZ9IY4vC5ZLtmuk5dHjHMVy4=";
        allowedIPs = [ "10.100.0.2/32" ];
      }
      {
        # paninix
        publicKey = "blQ/jJ3tKciOyuS/1TJSrRF9kU8gbtMrF8uhlwFWOCY=";
        allowedIPs = [ "10.100.0.3/32" ];
      }
      {
        # iPad
        publicKey = "tuMonMT00UkTneRrbn+0o4+jKvRYfWDSkVgqK7rOFUw=";
        allowedIPs = [ "10.100.0.4/32" ];
      }
    ];
  };
}
