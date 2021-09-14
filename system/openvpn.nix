{ pkgs, config, inputs, lib, ... }:
let
  mkNord = serv: {
    config = ''
      ${builtins.readFile "${inputs.nord-configs}/ovpn_tcp/${serv}.nordvpn.com.tcp.ovpn"}
      auth-user-pass ${config.age.secrets.nord-auth.path}
    '';
    autoStart = lib.mkDefault false;
  };

in
{
  age.secrets = {
    nord-auth.file = ../secrets/nord-auth.age;
    xc-openvpn-config.file = ../secrets/xc-openvpn-config.age;
    xc-openvpn-auth.file = ../secrets/xc-openvpn-auth.age;
  };
  services.openvpn.servers = {
    nord-hk = mkNord "hk-tw2";
    # nord-nl = mkNord "nl707" false;
    # nord-us = mkNord "us4629" false;
    nord-jp = mkNord "jp586";
    nord-ca = mkNord "ca977";
    cross-compass-vpn = {
      autoStart = false;
      config = ''
        config ${config.age.secrets.xc-openvpn-config.path}
        auth-user-pass ${config.age.secrets.xc-openvpn-auth.path}
      '';
    };
  };

}
