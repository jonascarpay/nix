{ pkgs, inputs, config, ... }:

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
  homekit-tcp-port = 21063; # Freely choosable
  homekit-udp-port = 5353; # Hardcoded by apple, I think

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
      serial.port = "/dev/ttyUSB0";
      frontend.port = z2m-port;
      homeassistant = config.services.home-assistant.enable;
      advanced.log_level = "debug";
    };
  };

  networking.firewall = {
    allowedTCPPorts = [ z2m-port ha-port nginx-port homekit-tcp-port ];
    allowedUDPPorts = [ homekit-udp-port ];
  };

  age.secrets.ha-secrets = {
    file = ../secrets/ha-secrets.age;
    path = "${config.services.home-assistant.configDir}/secrets.yaml";
    owner = "hass";
    group = "hass";
  };

  services.home-assistant = {
    enable = true;
    config = {
      homeassistant = {
        name = "Home";
        latitude = "!secret latitude";
        longitude = "!secret longitude";
        temperature_unit = "C";
        time_zone = config.time.timeZone;
        unit_system = "metric";
      };
      http = {
        server_port = ha-port;
        use_x_forwarded_for = true;
        trusted_proxies = [ "127.0.0.1" "::1" ];
      };
      homekit = {
        port = homekit-tcp-port;
        filter = {
          exclude_entity_globs = [ "automation.*" ];
        };
      };
      mqtt = { };
      netgear = { };
      scene = "!include scenes.yaml";
      automation = "!include automations.yaml";
      light = "!secret light_groups";
      group = {
        somebody_home = {
          name = "People";
          entities = "!secret phones";
          all = false;
        };
      };
      switch = [{
        platform = "flux";
        lights = "!secret flux_lights";
        disable_brightness_adjust = true;
      }];
      # default_config customization
      config = { };
      frontend = { };
      history = { };
      image = { };
      input_boolean = { };
      input_button = { };
      input_datetime = { };
      input_number = { };
      input_select = { };
      input_text = { };
      logbook = { };
      person = { };
      sun = { };
      system_health = { };
    };
    extraComponents = [
      # extras
      "homekit"
      "mqtt"
      "netgear"
      "group"
      "flux"
      # default_config customization
      "automation"
      "config"
      "frontend"
      "history"
      "image"
      "input_boolean"
      "input_button"
      "input_datetime"
      "input_number"
      "input_select"
      "input_text"
      "logbook"
      "person"
      "scene"
      "sun"
      "system_health"
    ];
    extraPackages = p: [ p.aiohomekit p.pyatv p.getmac p.async-upnp-client ];
    openFirewall = true;
  };
}
