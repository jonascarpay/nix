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
                PAPE=$(${pkgs.findutils}/bin/find ${cfg.wallpaperPath} -type f | ${pkgs.coreutils}/bin/shuf -n 1);
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
        Timer.OnUnitActiveSec = cfg.interval;
        Install.WantedBy = [ "timers.target" ];
      };
    };
  };
}
