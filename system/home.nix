{ pkgs, inputs, ... }:

let
  cc2531-firmware = pkgs.fetchzip {
    url = "https://github.com/Koenkk/Z-Stack-firmware/raw/master/coordinator/Z-Stack_Home_1.2/bin/default/CC2531_DEFAULT_20211115.zip";
    sha256 = "sha256-WY2KXNbDnAUTjiuCW0AtVjrC1dhPKNVCYefE84mk2Hw=";
    stripRoot = false;
  };

  flash = pkgs.writeShellApplication {
    name = "cc2531-flash";
    runtimeInputs = [ pkgs.cc-tool ];
    text = ''
      set -e
      sudo cc-tool -e -w ${cc2531-firmware}/CC2531ZNP-Prod.hex
    '';
  };

  z2m-port = 8091;
  ha-port = 8123;
  nginx-port = 80;

in
{
  environment.systemPackages = [ flash ];

  services.nginx = {
    enable = true;
    virtualHosts = {
      # adapted from https://www.zigbee2mqtt.io/guide/configuration/frontend.html#nginx-proxy-configuration
      # TODO The guide has a separate location set up for /api, but this appears unnecessary?
      "zigbee.onigiri.lan" = {
        listen = [{ addr = "0.0.0.0"; port = nginx-port; }];
        locations."/" = {
          proxyPass = "http://localhost:${builtins.toString z2m-port}";
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_http_version 1.1; # Necessary for keepalive
            proxy_set_header Upgrade $http_upgrade; # with Connection, necessary for websocket http://nginx.org/en/docs/http/websocket.html
            proxy_set_header Connection "upgrade";
          '';
        };
      };
      # This is currently the same as zigbee.onigiri.lan, but I don't want to
      # abstract it because it might not always be true
      # TODO The guide has a separate location set up for /api/websocket, but this appears unnecessary?
      "home.onigiri.lan" = {
        listen = [{ addr = "0.0.0.0"; port = nginx-port; }];
        locations."/" = {
          proxyPass = "http://localhost:${builtins.toString ha-port}";
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
          '';
        };
      };
    };
  };

  services.mosquitto = {
    enable = true;
    listeners = [ ];
  };

  services.zigbee2mqtt = {
    package = pkgs.callPackage "${inputs.unstable}/pkgs/servers/zigbee2mqtt" { };
    enable = true;
    settings = {
      permit_join = true;
      serial.port = "/dev/ttyACM0";
      frontend.port = z2m-port;
      homeassistant = true;
    };
  };

  networking.firewall.allowedTCPPorts = [ z2m-port ha-port nginx-port ];

  services.home-assistant = {
    enable = true;
    config = {
      homeassistant = {
        name = "Home";
        latitude = 35.6762;
        longitude = 139.6503;
        temperature_unit = "C";
        time_zone = "Asia/Tokyo";
        unit_system = "metric";
      };
      http = {
        server_port = ha-port;
        use_x_forwarded_for = true;
        trusted_proxies = [ "127.0.0.1" "::1" ];
      };
      default_config = { };
      homekit = { };
      mqtt = { };
      netgear = { };
    };
    extraComponents = [ "default_config" "homekit" "mqtt" "netgear" ];
    openFirewall = true;
  };
}
