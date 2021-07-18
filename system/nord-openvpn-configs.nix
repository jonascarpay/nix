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
        unzip -q ovpn.zip
        rm ovpn.zip
        git status
        CHANGED=$(git diff-index --name-only HEAD --)
        if [ -n "$CHANGED" ]; then
            git add --all
            git commit -m "Nord configs at $DATE"
            git push
        fi
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
