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

in
{
  environment.systemPackages = [ flash ];

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

  networking.firewall.allowedTCPPorts = [ z2m-port ];

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
      http = { };
      default_config = { };
      # met = { };
      homekit = { };
      # google_assistant = { };
      # mqtt = { };
      netgear = { };
    };
    extraComponents = [ "default_config" "homekit" "mqtt" "netgear" ];
    openFirewall = true;
  };
}
