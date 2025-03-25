{ pkgs, ... }:
{
  systemd.user = {
    services.swww-daemon = {
      Install = { WantedBy = [ "graphical-session.target" ]; };
      Unit = {
        ConditionEnvironment = "WAYLAND_DISPLAY";
        Description = "swww-daemon";
        After = [ "graphical-session.target" "niri.service" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = "${pkgs.swww}/bin/swww-daemon";
        Restart = "always";
        RestartSec = 10;
      };
    };
    services.random-wallpaper = {
      Unit = {
        Description = "Set random wallpaper using swww";
        After = [ "swww-daemon.service" ];
        Requires = [ "swww-daemon.service" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart =
          let
            script = pkgs.writeShellScript "random-wallpaper" ''
              set -eo pipefail
              PAPE=$(${pkgs.findutils}/bin/find $HOME/Wallpapers/papes/ -type f | ${pkgs.coreutils}/bin/shuf -n 1);
              echo "Setting wallpaper to $PAPE"
              ${pkgs.swww}/bin/swww img "$PAPE"
            '';
          in
          "${script}";
        IOSchedulingClass = "idle";
      };
      Install.WantedBy = [ "swww-daemon.service" ];
    };
    timers.random-wallpaper = {
      Unit.Description = "Set random wallpaper using swww";
      Timer.OnUnitActiveSec = "1h";
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
