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
    services.awww.enable = true;

    systemd.user = {
      services.random-wallpaper = {
        Unit = {
          Description = "Set random wallpaper using awww";
          After = [ "awww.service" ];
          Requires = [ "awww.service" ];
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
        Install.WantedBy = [ "awww.service" ];
      };

      timers.random-wallpaper = {
        Unit.Description = "Set random wallpaper using awww";
        Timer.OnUnitActiveSec = cfg.interval;
        Install.WantedBy = [ "timers.target" ];
      };
    };
  };
}
