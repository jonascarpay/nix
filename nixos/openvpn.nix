{ config, ... }:
{
  age.secrets = {
    xc-openvpn-config.file = ../secrets/xc-openvpn-config.age;
    xc-openvpn-auth.file = ../secrets/xc-openvpn-auth.age;
    ndh-openvpn.file = ../secrets/ndh-openvpn.age;
  };
  services.openvpn.servers = {
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
    ndh-vpn = {
      autoStart = false;
      config = "config ${config.age.secrets.ndh-openvpn.path}";
    };
  };
}
