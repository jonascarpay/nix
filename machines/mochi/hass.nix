{ config, lib, ... }:
let

  hass-port = 8123;
  z2m-port = 8091;

  todo = {
    services.home-assistant = {
      extraComponents = [
        "todo"
        "local_todo"
      ];
      # config.todo = { };
    };
  };

in

{

  imports = [
    todo
  ];

  services.mosquitto.enable = true;

  networking.firewall.allowedTCPPorts = [ hass-port z2m-port ];
  age.secrets.ha-secrets = {
    file = ../../secrets/ha-secrets.age;
    path = "${config.services.home-assistant.configDir}/secrets.yaml";
    owner = "hass";
    group = "hass";
  };

  services.zigbee2mqtt = {
    enable = true;
    settings = {
      permit_join = true;
      serial.port = "/dev/ttyUSB0";
      frontend = {
        port = z2m-port;
        # url = "http://zigbee.onigiri.lan";
      };
      availability.enabled = true;
      homeassistant = lib.optionalAttrs config.services.home-assistant.enable {
        enabled = true;
        # discovery_topic = "homeassistant";
        # status_topic = "hass/status";
        # legacy_entity_attributes = true;
        # legacy_triggers = true;
      };
      # advanced.log_level = "debug";
    };
  };

  services.home-assistant = {
    enable = true;
    openFirewall = true;
    configWritable = true;
    config = {
      homeassistant = {
        name = "Home";
        latitude = "!secret latitude";
        longitude = "!secret longitude";
        time_zone = config.time.timeZone;
        unit_system = "metric";
      };
      http.server_port = hass-port;
      config = { };
      history = { };
      sun = { };
      automation = "!include automations.yaml";
      scene = "!include scenes.yaml";
    };
    extraComponents = [
      "history" # entity history
      "met" # weather
      "mqtt"
      "group"
      "calendar"
      "sun"
      "homekit"
      "wake_on_lan"
      "automation"
      "recorder"
    ];
  };
}
