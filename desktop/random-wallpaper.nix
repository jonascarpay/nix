{ config, lib, pkgs, ... }:
let
  cfg = config.services.randomWallpaper;
in
{
  options.services.randomWallpaper = {
    enable = lib.mkEnableOption "Random wallpaper service";

    wallpaperPath = lib.mkOption {
      type = lib.types.str;
      description = "Path to the wallpaper directory";
    };

    interval = lib.mkOption {
      type = lib.types.str;
      default = "1h";
      example = "30min";
      description = "Interval between wallpaper changes";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.user = {
      # TODO awww is now in home manager
      services.awww-daemon = {
        Install = { WantedBy = [ "graphical-session.target" ]; };
        Unit = {
          ConditionEnvironment = "WAYLAND_DISPLAY";
          Description = "awww-daemon";
          After = [ "graphical-session.target" "niri.service" ];
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          ExecStart = "${pkgs.awww}/bin/awww-daemon";
          Restart = "always";
          RestartSec = 10;
        };
      };

      services.random-wallpaper = {
        Unit = {
          Description = "Set random wallpaper using awww";
          After = [ "awww-daemon.service" ];
          Requires = [ "awww-daemon.service" ];
        };
        Service = {
          Type = "oneshot";
          ExecStart =
            let
              script = pkgs.writeShellScript "random-wallpaper" ''
                set -eo pipefail
                PAPE=$(${pkgs.findutils}/bin/find ${cfg.wallpaperPath} -type f | ${pkgs.coreutils}/bin/shuf -n 1);
                echo "Setting wallpaper to $PAPE"
                ${pkgs.awww}/bin/awww img "$PAPE"
              '';
            in
            "${script}";
          IOSchedulingClass = "idle";
        };
        Install.WantedBy = [ "awww-daemon.service" ];
      };

      timers.random-wallpaper = {
        Unit.Description = "Set random wallpaper using awww";
        Timer.OnUnitActiveSec = cfg.interval;
        Install.WantedBy = [ "timers.target" ];
      };
    };
  };
}
