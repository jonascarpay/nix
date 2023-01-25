{ pkgs, config, inputs, lib, ... }:
let
  mkNord = serv: {
    config = ''
      config ${inputs.nord-configs}/ovpn_tcp/${serv}.nordvpn.com.tcp.ovpn
      auth-user-pass ${config.age.secrets.nord-auth.path}
    '';
    autoStart = false;
  };

in
{
  age.secrets = {
    nord-auth.file = ../secrets/nord-auth.age;
    xc-openvpn-config.file = ../secrets/xc-openvpn-config.age;
    xc-openvpn-auth.file = ../secrets/xc-openvpn-auth.age;
  };
  services.openvpn.servers = {
    nord = mkNord "jp590";
    offshore = mkNord "hk248";
    us = mkNord "us9041";
    cross-compass-vpn = {
      autoStart = false;
      config = ''
        config ${config.age.secrets.xc-openvpn-config.path}
        auth-user-pass ${config.age.secrets.xc-openvpn-auth.path}
        route-nopull
        route 192.168.2.0 255.255.255.0 vpn_gateway
        route 192.168.100.0 255.255.255.0 vpn_gateway
      '';
    };
  };
}
