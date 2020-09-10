{ pkgs, config, ... }:
let
  inherit (config.secrets.spotify) username password;
in
{
  home.packages = [
    pkgs.spotify-tui
  ];


  systemd.user.services.spotifyd = {
    Unit.Description = "spotifyd service";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = ''
        ${pkgs.spotifyd}/bin/spotifyd --no-daemon -u "${username}" -p "${password}"
      '';
    };
  };
}
