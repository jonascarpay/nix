{ pkgs, ... }: {
  systemd.user = {
    services.random-wallpaper = {
      Unit.Description = "Set random wallpaper using feh";
      Unit.After = [ "graphical-session-pre.target" ];
      Unit.PartOf = [ "graphical-session.target" ];
      Service.Type = "oneshot";
      Service.ExecStart =
        let
          script = pkgs.writeShellScript "random-wallpaper" ''
            set -eo pipefail
            PAPE=$(${pkgs.findutils}/bin/find $HOME/Wallpapers/papes/ -type f | ${pkgs.coreutils}/bin/shuf -n 1);
            echo "Setting wallpaper to $PAPE"
            ${pkgs.feh}/bin/feh --bg-fill --no-fehbg "$PAPE"
          '';
        in
        "${script}";
      Service.IOSchedulingClass = "idle";
      Install.WantedBy = [ "graphical-session.target" ];
    };
    timers.random-wallpaper = {
      Unit.Description = "Set random wallpaper using feh";
      Timer.OnUnitActiveSec = "1h";
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
