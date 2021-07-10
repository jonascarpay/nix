{ pkgs, ... }:
let
  configsDir = "/home/jmc/nord-openvpn-configs";
  configsUrl = "https://downloads.nordcdn.com/configs/archives/servers/ovpn.zip";
  script =
    let
      curl = "${pkgs.curl}/bin/curl";
      unzip = "${pkgs.unzip}/bin/unzip";
      git = "${pkgs.git}/bin/git";
      rm = "${pkgs.coreutils}/bin/rm";
      date = "${pkgs.coreutils}/bin/date";
    in
    pkgs.writeShellScript "nord-openvpn-configs" ''
      set -eux
      cd ${configsDir}
      ${rm} -rf ovpn_tcp
      ${rm} -rf ovpn_udp
      DATE=$(${date} -u)
      ${curl} "${configsUrl}" --output ovpn.zip
      ${unzip} ovpn.zip
      ${rm} ovpn.zip
      ${git} add --all
      ${git} commit -m "Nord configs at $DATE"
      ${git} push
    '';
in
{
  systemd.user.services.nord-openvpn-configs-sync = {
    Unit.Description = "Nord openvpn configs sync";
    Service = {
      Type = "oneshot";
      ExecStart = "${script}";
    };
  };
  systemd.user.timers.nord-openvpn-configs-sync = {
    Unit.Description = "Nord openvpn configs sync timer";
    Timer.OnCalendar = "daily";
    Install.WantedBy = [ "timers.target" ];
  };
}
