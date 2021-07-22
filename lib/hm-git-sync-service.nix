{ pkgs
, name
, dir
, description ? name
, time ? "*:0/15"
}:
# https://github.com/simonthum/git-sync
{
  systemd.user = {
    services.${name} = {
      Unit.Description = description;
      Service = {
        Type = "oneshot";
        ExecStart =
          let script = pkgs.writeShellScript "${name}" ''
            cd ${dir}
            ${pkgs.gitAndTools.git-sync}/bin/git-sync -n -s sync
          '';
          in "${script}";
      };
    };
    timers.${name} = {
      Unit.Description = "${description} timer";
      Timer.OnCalendar = time;
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
