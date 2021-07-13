{ pkgs, ... }:
let
  configsDir = "/home/jmc/nord-openvpn-configs";
  configsUrl = "https://downloads.nordcdn.com/configs/archives/servers/ovpn.zip";
in
{
  systemd = {
    services.nord-openvpn-configs-sync = {
      description = "Nord openvpn configs sync";
      path = with pkgs; [ curl unzip git ];
      script = ''
        set -eux
        cd ${configsDir}
        rm -rf ovpn_tcp
        rm -rf ovpn_udp
        DATE=$(date -u)
        curl "${configsUrl}" --output ovpn.zip
        unzip ovpn.zip
        rm ovpn.zip
        git add --all
        git commit -m "Nord configs at $DATE" || true
        git push
      '';
      serviceConfig.Type = "oneshot";
      serviceConfig.User = "jmc";
    };
    timers.nord-openvpn-configs-sync = {
      description = "Nord openvpn configs sync timer";
      wantedBy = [ "default.target" "timers.target" ];
      timerConfig.OnCalendar = "daily";
    };
  };
}
